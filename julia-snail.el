;;; julia-snail.el --- Julia Snail -*- lexical-binding: t -*-


;;; --- requirements

(require 'cl-lib)
(require 'cl-macs)
(require 'json)
(require 's)
(require 'spinner)
(require 'subr-x)
(require 'thingatpt)
(require 'vterm)

(require 'julia-snail-parser "parser.el")


;;; --- customization

(defgroup julia-snail nil
  "Customization options for Julia Snail mode."
  :group 'external)

(defcustom julia-snail-executable "julia"
  "Julia executable to run as a Snail server."
  :tag "Julia executable"
  :group 'julia-snail
  :safe 'stringp
  :type 'string)

(defcustom julia-snail-port 10011
  "Default Snail server port."
  :tag "Snail server port"
  :group 'julia-snail
  :safe 'integerp
  :type 'integer)
(make-variable-buffer-local 'julia-snail-port)

(defcustom julia-snail-repl-buffer "*julia*"
  "Default buffer to use for Julia REPL interaction."
  :tag "Julia REPL buffer"
  :group 'julia-snail
  :safe 'stringp
  :type 'string)
(make-variable-buffer-local 'julia-snail-buffer)

(defcustom julia-snail-show-error-window t
  "When t, show compilation errors in separate window. When nil,
just display them in the minibuffer."
  :tag "Show compilation errors in separate window"
  :group 'julia-snail
  :type 'boolean)


;;; --- variables

(defvar-local julia-snail--process nil)

(defvar julia-snail--server-file
  (concat (if load-file-name
                (file-name-directory load-file-name)
              (file-name-as-directory default-directory))
          "JuliaSnail.jl"))

(defvar julia-snail--requests
  (make-hash-table :test #'equal))

(defvar julia-snail--proc-responses
  (make-hash-table :test #'equal))


;;; --- Snail protocol request tracking data structure

(cl-defstruct julia-snail--request-tracker
  repl-buf
  originating-buf
  (callback-success (lambda (&optional data) (message "Snail command succeeded")))
  (callback-failure (lambda () (message "Snail command failed")))
  tmpfile)


;;; --- supporting functions

(defun julia-snail--process-buffer-name (repl-buf)
  (let ((real-buf (get-buffer repl-buf)))
    (unless real-buf
      (error "no REPL buffer found"))
    (format "%s process" (buffer-name (get-buffer real-buf)))))

(defun julia-snail--error-buffer (repl-buf error-message error-stack)
  (let ((real-buf (get-buffer repl-buf)))
    (unless real-buf
      (error "no REPL buffer found"))
    (let* ((error-buf-name (format "%s error" (buffer-name (get-buffer real-buf))))
           (error-buf (get-buffer-create error-buf-name)))
      (with-current-buffer error-buf
        (read-only-mode -1)
        (erase-buffer)
        (insert error-message)
        (insert "\n\n")
        (insert (s-join "\n" error-stack))
        (goto-char (point-min))
        (read-only-mode 1)
        (julia-snail-error-buffer-mode))
      error-buf)))

(defun julia-snail--flash-region (start end &optional timeout)
  ;; borrowed from SLIME
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun julia-snail--construct-module-path (module)
  "Return a Julia array representing the module path, as Julia
symbols, given by MODULE. MODULE can be:
- nil, which returns [:Main]
- an Elisp keyword, which returns [<keyword>], including the
  leading colon in the keyword
- an Elisp list, which can contain either keywords or strings,
  and which is converted to a Julia array literal with the
  entries of the input list converted to Julia keywords"
  (cond ((null module) "[:Main]")
        ((keywordp module) (format "[%s]" module))
        ((listp module) (format
                         "[%s]"
                         (s-join " " (-map (lambda (s)
                                             (if (keywordp s)
                                                 (format "%s" s)
                                               (format ":%s" s)))
                                           module))))
        (t (error "Malformed module specification"))))

(defun julia-snail--identifier-at-point ()
  (let ((stab (copy-syntax-table)))
    (with-syntax-table stab
      (modify-syntax-entry ?. "_")
      (modify-syntax-entry ?@ "_")
      (modify-syntax-entry ?= " ")
      (thing-at-point 'symbol t))))

(defmacro julia-snail--wait-while (condition increment maximum)
  (let ((sleep-total (gensym))
        (incr (gensym))
        (max (gensym)))
    `(let ((,sleep-total 0)
           (,incr ,increment)
           (.max ,maximum))
       (while (and (< ,sleep-total ,maximum) ,condition)
         (sleep-for 0 ,incr)
         (setf ,sleep-total (+ ,sleep-total ,incr))))))


;;; --- connection management functions

(defun julia-snail--repl-cleanup ()
  (let ((process-buf (get-buffer (julia-snail--process-buffer-name (current-buffer)))))
    (when process-buf
      (kill-buffer process-buf)))
  (setq julia-snail--process nil))

(defun julia-snail--repl-enable ()
  (add-hook 'kill-buffer-hook #'julia-snail--repl-cleanup nil t)
  (make-local-variable 'julia-snail--repl-go-back-target)
  (let ((repl-buf (current-buffer))
        (process-buf (get-buffer-create (julia-snail--process-buffer-name (current-buffer)))))
    (when (fboundp #'persp-add-buffer) ; perspective-el support
      (persp-add-buffer process-buf))
    (with-current-buffer process-buf
      (unless julia-snail--process
        (setq julia-snail-port (buffer-local-value 'julia-snail-port repl-buf))
        ;; XXX: This is currently necessary because there does not appear to be
        ;; a way to pass arguments to an interactive Julia session. This does
        ;; not work: `julia -L JuliaSnail.jl -- $PORT`.
        ;; https://github.com/JuliaLang/julia/issues/10226 refers to this
        ;; problem and supposedly fixes it, but it does not work for me with
        ;; Julia 1.0.4.
        ;; TODO: Follow-up on https://github.com/JuliaLang/julia/issues/33752
        (julia-snail--send-to-repl repl-buf
          (format "JuliaSnail.start(%d);" julia-snail-port)
          :async nil)
        (with-current-buffer repl-buf
          (setq julia-snail--process ; NB: buffer-local variable!
                (open-network-stream "julia-process" process-buf "localhost" julia-snail-port))
          (set-process-filter julia-snail--process #'julia-snail--server-response-filter))))))

(defun julia-snail--repl-disable ()
  (julia-snail--repl-cleanup))

(defun julia-snail--enable ()
  ;; placeholder for source buffer minor mode initialization
  )

(defun julia-snail--disable ()
  ;; placeholder for source buffer minor mode cleanup
  )


;;; --- Julia REPL and Snail server interaction functions

(cl-defun julia-snail--send-to-repl (repl-buf str &key (async t))
  "Insert str directly into the REPL buffer. When :async is nil,
wait for the REPL prompt to return, otherwise return immediately."
  (declare (indent defun))
  (unless repl-buf
    (error "no REPL buffer given"))
  (with-current-buffer repl-buf
    (vterm-send-string str)
    (vterm-send-return)
    (unless async
      ;; wait for the inclusion to succeed (i.e., the prompt prints)
      (julia-snail--wait-while (not (string-equal "julia>" (current-word))) 20 5000))))

(cl-defun julia-snail--send-to-server
    (repl-buf module str &key callback-success callback-failure)
  "Send str to Snail server."
  (declare (indent defun))
  (unless repl-buf
    (error "no REPL buffer given"))
  (let* ((process-buf (get-buffer (julia-snail--process-buffer-name repl-buf)))
         (module-ns (julia-snail--construct-module-path module))
         (reqid (format "%04x%04x" (random (expt 16 4)) (random (expt 16 4))))
         (msg (format "(ns = %s, reqid = \"%s\", code = %s)\n"
                      module-ns
                      reqid
                      (json-encode-string str))))
    (with-current-buffer process-buf
      (goto-char (point-max))
      (insert msg))
    (process-send-string process-buf msg)
    (spinner-start 'progress-bar)
    (puthash reqid
             (make-julia-snail--request-tracker
              :repl-buf repl-buf
              :originating-buf (current-buffer)
              :callback-success callback-success
              :callback-failure callback-failure)
             julia-snail--requests)
    reqid))

(cl-defun julia-snail--send-to-server-via-tmp-file
    (repl-buf module str &key callback-success callback-failure)
  "Send str to server by first writing it to a tmpfile, calling
Julia include on the tmpfile, and then deleting the file."
  (declare (indent defun))
  (unless repl-buf
    (error "no REPL buffer given"))
  (let ((text (s-trim str))
        (tmpfile (make-temp-file
                  (expand-file-name "julia-tmp"
                                    (or small-temporary-file-directory
                                        temporary-file-directory)))))
    (progn
      (with-temp-file tmpfile
        (insert text))
      (let ((reqid (julia-snail--send-to-server
                     repl-buf
                     module
                     (format "include(\"%s\");" tmpfile)
                     :callback-success callback-success
                     :callback-failure callback-failure)))
        (puthash reqid
                 (make-julia-snail--request-tracker
                  :repl-buf repl-buf
                  :originating-buf (current-buffer)
                  :callback-success callback-success
                  :callback-failure callback-failure
                  :tmpfile tmpfile)
                 julia-snail--requests)))))

(defun julia-snail--server-response-filter (proc str)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; insert at the end unconditionally
      (goto-char (point-max))
      (insert str)
      (set-marker (process-mark proc) (point))
      ;; Need to read and eval the value sent in by the process (str). But it
      ;; may have been chunked. Assume that a successful read signals the end of
      ;; input, but a failed read needs to be concatenated to other upcoming
      ;; reads. Track them in a table hashed by the proc.
      (let ((candidate (concatenate 'string
                                    (gethash proc julia-snail--proc-responses)
                                    str)))
        (condition-case nil
            (let ((read-str (read candidate)))
              ;; read succeeds, so clean up and return its eval value
              (remhash proc julia-snail--proc-responses)
              ;; scary
              (eval read-str))
          ;; read failed: this means more data is incoming
          (end-of-file
           (puthash proc candidate julia-snail--proc-responses)))))))


 ;;; --- Snail server response handling functions

(defun julia-snail--response-base (reqid)
  (let ((request-info (gethash reqid julia-snail--requests)))
    (when request-info
      ;; tmpfile
      (when-let (tmpfile (julia-snail--request-tracker-tmpfile request-info))
        (delete-file tmpfile))
      ;; stop spinner
      (with-current-buffer (julia-snail--request-tracker-originating-buf request-info)
        (spinner-stop))
      ;; remove request ID from requests hash
      (remhash reqid julia-snail--requests))))

(defun julia-snail--response-success (reqid result-data)
  (let* ((request-info (gethash reqid julia-snail--requests))
         (callback-success (julia-snail--request-tracker-callback-success request-info)))
    (when callback-success
      (funcall callback-success result-data)))
  (julia-snail--response-base reqid))

(defun julia-snail--response-failure (reqid error-message error-stack)
  (if (not julia-snail-show-error-window)
      (message error-message)
    (let* ((request-info (gethash reqid julia-snail--requests))
           (repl-buf (julia-snail--request-tracker-repl-buf request-info))
           (error-buffer (julia-snail--error-buffer repl-buf error-message error-stack))
           (callback-failure (julia-snail--request-tracker-callback-failure request-info)))
      (display-buffer error-buffer)
      (when callback-failure
        (funcall callback-failure))))
  (julia-snail--response-base reqid))


;;; --- xref implementation

(defun julia-snail--xref-backend-identifiers (callback-success)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (if (null repl-buf)
        (error "No Julia REPL buffer %s found; run julia-snail" julia-snail-repl-buffer)
      (let* ((module (julia-snail-parser-query (current-buffer) (point) :module)))
        (julia-snail--send-to-server repl-buf
          module
          (format "JuliaSnail.xref_backend_identifiers(%s)" module)
          :callback-success callback-success)))))

(defun julia-snail--xref-backend-definitions (identifier callback-success)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (if (null repl-buf)
        (error "No Julia REPL buffer %s found; run julia-snail" julia-snail-repl-buffer)
      (if (null identifier)
          (error "No identifier at point")
        (let* ((module (julia-snail-parser-query (current-buffer) (point) :module))
               ;; Grab everything in the identifier up to the last dot, i.e.,
               ;; the fully-qualified module name, and everything after the
               ;; last dot, which should be the symbol in the module.
               (identifier-split (save-match-data
                                   (if (string-match
                                        "\\(.*\\)\\.\\(.*\\)"
                                        identifier)
                                       (list (match-string 1 identifier)
                                             (match-string 2 identifier))
                                     (list module identifier))))
               (identifier-ns (-first-item identifier-split))
               (identifier-name (-second-item identifier-split)))
          (julia-snail--send-to-server repl-buf
            module
            (format "JuliaSnail.xref_backend_definitions(%s, \"%s\")"
                    identifier-ns identifier-name)
            :callback-success callback-success))))))

(defun julia-snail-xref-backend ()
  'xref-julia-snail)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-julia-snail)))
  (julia-snail--identifier-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-julia-snail)))
  (let ((res nil))
    ;; Kick off async request to the Snail server. The success callback will
    ;; destructively modify the closed-over res variable, which this method
    ;; polls.
    (julia-snail--xref-backend-identifiers
     (lambda (&optional data)
       (setq res (or data :nothing))))
    (julia-snail--wait-while (null res) 20 1000)
    res))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-julia-snail)) identifier)
  (let ((res nil))
    ;; Kick off async request to the Snail server. The success callback will
    ;; destructively modify the closed-over res variable, which this method
    ;; polls.
    (julia-snail--xref-backend-definitions identifier
                                           (lambda (&optional data)
                                             (setq res (or data :nothing))))
    (julia-snail--wait-while (null res) 20 1000)
    ;; process res
    (if (or (null res) (eq :nothing res))
        nil
      (mapcar (lambda (candidate)
                (let ((descr (-first-item candidate))
                      (path (-second-item candidate))
                      (line (-third-item candidate)))
                  (xref-make descr
                             (if (file-exists-p path)
                                 (xref-make-file-location path line 0)
                               (xref-make-bogus-location
                                "xref not supported for definitions evaluated with julia-snail-send-top-level-form")))))
              res))))

(cl-defmethod xref-backend-references ((_backend (eql xref-julia-snail)) identifier)
  ;; Not sure I want to support this.
  nil)

(cl-defmethod xref-backend-apropos ((_backend (eql xref-julia-snail)) pattern)
  ;; ...
  )


;;; --- commands

;;;###autoload
(defun julia-snail ()
  "Start a Julia REPL and connect to it, or switch if one already exists.
The following buffer-local variables control it:
- julia-snail-repl-buffer (default: *julia*)
- julia-snail-port (default: 10011)
To create multiple REPLs, give these variables distinct values (e.g.:
*julia my-project-1* and 10012)."
  (interactive)
  (let ((source-buf (current-buffer))
        (repl-buf (get-buffer julia-snail-repl-buffer)))
    (if repl-buf
        (progn
          (setf (buffer-local-value 'julia-snail--repl-go-back-target repl-buf) source-buf)
          (pop-to-buffer-same-window repl-buf))
      ;; run Julia in a vterm and load the Snail server file
      (let ((vterm-shell (format "%s -L %s" julia-snail-executable julia-snail--server-file)))
        (vterm julia-snail-repl-buffer)
        (setq-local julia-snail-port (buffer-local-value 'julia-snail-port source-buf))
        (setq-local julia-snail--repl-go-back-target source-buf)
        (julia-snail-repl-mode)))))

(defun julia-snail-send-line ()
  "Copy the line at the current point into the REPL and run it.
This is not module-context aware."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (if (null repl-buf)
        (error "No Julia REPL buffer %s found; run julia-snail" julia-snail-repl-buffer)
      (let ((line (s-trim (thing-at-point 'line t))))
        (julia-snail--send-to-repl repl-buf
          line)))))

(defun julia-snail-send-buffer ()
  "Send the current buffer's file into the Julia REPL, and include() it.
This will occur in the context of the Main module, just as it would at the REPL."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer))
        (filename buffer-file-name))
    (if (null repl-buf)
        (error "No Julia REPL buffer %s found; run julia-snail" julia-snail-repl-buffer)
      (julia-snail--send-to-server repl-buf
        :Main
        (format "include(\"%s\");" filename)
        :callback-success (lambda (&optional data)
                            (message "%s loaded" filename))))))

(defun julia-snail-send-region ()
  "Send the region (requires transient-mark) to the Julia REPL and evaluate it.
This occurs in the context of the current module."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (if (null repl-buf)
        (error "No Julia REPL buffer %s found; run julia-snail" julia-snail-repl-buffer)
      (if (null (use-region-p))
          (error "No region selected")
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end)))
              (module (julia-snail-parser-query (current-buffer) (point) :module)))
          (julia-snail--send-to-server-via-tmp-file repl-buf
            module text
            :callback-success (lambda (&optional data)
                                (message "Selected region evaluated: module %s, result: %s"
                                         (julia-snail--construct-module-path module)
                                         data))))))))

(defun julia-snail-send-top-level-form ()
  "Send the top-level form surrounding the point to the Julia REPL and evaluate it.
This occurs in the context of the current module."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (if (null repl-buf)
        (error "No Julia REPL buffer %s found; run julia-snail" julia-snail-repl-buffer)
      (let* ((q (julia-snail-parser-query (current-buffer) (point) :top-level-block))
             (module (plist-get q :module))
             (block-description (plist-get q :block))
             (block-start (-second-item block-description))
             (block-end (-third-item block-description))
             (text (buffer-substring-no-properties block-start block-end)))
        (julia-snail--flash-region block-start block-end 0.5)
        (julia-snail--send-to-server-via-tmp-file repl-buf
          module text
          :callback-success (lambda (&optional data)
                              (message "Top-level form evaluated: module %s, %s"
                                       (julia-snail--construct-module-path module)
                                       (if (-fourth-item block-description)
                                           (-fourth-item block-description)
                                         "unknown"))))))))

(defun julia-snail-package-activate (dir)
  "Activate a Pkg project in the Julia REPL."
  (interactive "DProject directory: ")
  (let ((repl-buf (get-buffer julia-snail-repl-buffer))
        (expanded-dir (expand-file-name dir)))
    (if (null repl-buf)
        (error "No Julia REPL buffer %s found; run julia-snail" julia-snail-repl-buffer)
      (julia-snail--send-to-server repl-buf
        :Main
        (format "Pkg.activate(\"%s\")" expanded-dir)
        :callback-success (lambda (&optional data)
                            (message "Package activated: %s" expanded-dir))))))

(defun julia-snail-repl-go-back ()
  (interactive)
  (when (boundp 'julia-snail--repl-go-back-target)
    (pop-to-buffer julia-snail--repl-go-back-target 'display-buffer-reuse-window)))


;;; --- keymaps

(defvar julia-snail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'julia-snail)
    (define-key map (kbd "C-c C-a") #'julia-snail-package-activate)
    (define-key map (kbd "C-c C-c") #'julia-snail-send-top-level-form)
    (define-key map (kbd "C-M-x") #'julia-snail-send-top-level-form)
    (define-key map (kbd "C-c C-r") #'julia-snail-send-region)
    (define-key map (kbd "C-c C-l") #'julia-snail-send-line)
    (define-key map (kbd "C-c C-k") #'julia-snail-send-buffer)
    map))

(defvar julia-snail-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'julia-snail-repl-go-back)
    map))


;;; --- mode definitions

;;;###autoload
(define-minor-mode julia-snail-mode
  "A minor mode for interactive Julia development. Should only be turned on in source buffers."
  :init-value nil
  :lighter " Snail"
  :keymap julia-snail-mode-map
  (when (eq 'julia-mode major-mode)
    (if julia-snail-mode
        (progn
          (julia-snail--enable)
          (add-hook 'xref-backend-functions #'julia-snail-xref-backend nil t))
      (remove-hook 'xref-backend-functions #'julia-snail-xref-backend t)
      (julia-snail--disable))))

;;;###autoload
(define-minor-mode julia-snail-repl-mode
  "A minor mode for interactive Julia development. Should only be
turned on in REPL buffers."
  :init-value nil
  :lighter " Snail"
  :keymap julia-snail-repl-mode-map
  (when (eq 'vterm-mode major-mode)
    (if julia-snail-repl-mode
        (julia-snail--repl-enable)
      (julia-snail--repl-disable))))

(define-minor-mode julia-snail-error-buffer-mode
  "A minor mode for displaying errors returned from the Julia REPL."
  :init-value nil
  :lighter " Snail Error"
  :keymap '(((kbd "q") . quit-window)))

(provide 'julia-snail)
