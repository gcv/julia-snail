;;; julia-snail-mode.el --- Julia Snail -*- lexical-binding: t -*-


;;; --- requirements

(require 'cl-lib)
(require 'json)
(require 's)
(require 'thingatpt)
(require 'vterm)


;;; --- customization

(defgroup julia-snail nil
  "Customization options for Julia Snail mode."
  :group 'external)


(defcustom julia-snail-executable "julia"
  "Julia executable to run as a Snail server."
  :tag "Julia executable"
  :group 'julia-snail
  :type 'string)


(defcustom julia-snail-port 2001
  "Snail server port."
  :tag "Snail server port"
  :group 'julia-snail
  :type 'integer)


(defcustom julia-snail-repl-buffer "*julia-repl*"
  "Buffer to use for Julia REPL interaction."
  :tag "Julia REPL buffer"
  :group 'julia-snail
  :type 'string)


;;; --- variables

(defvar-local julia-snail--client nil)

(defvar julia-snail--server-file
  (concat (file-name-directory load-file-name) "JuliaSnail.jl"))

(defvar julia-snail--requests
  (make-hash-table :test #'equal))


;;; --- supporting functions

(defun julia-snail--client-buffer-name (repl-buf)
  (let ((real-buf (get-buffer repl-buf)))
    (unless real-buf
      (error "no REPL buffer found"))
    (format "%s client" (buffer-name (get-buffer real-buf)))))


;;; --- connection management functions

(defun julia-snail--cleanup ()
  (let ((client-buf (get-buffer (julia-snail--client-buffer-name (current-buffer)))))
    (when client-buf
      (kill-buffer client-buf)))
  (setq julia-snail--client nil))


(defun julia-snail--enable ()
  (add-hook 'kill-buffer-hook #'julia-snail--cleanup nil t)
  (let ((repl-buf (current-buffer))
        (client-buf (get-buffer-create (julia-snail--client-buffer-name (current-buffer)))))
    (when (fboundp #'persp-add-buffer) ; perspective-el support
      (persp-add-buffer client-buf))
    (with-current-buffer client-buf
      (unless julia-snail--client
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
          (setq julia-snail--client ; NB: buffer-local variable!
                (open-network-stream "julia-client" client-buf "localhost" julia-snail-port))
          (set-process-filter julia-snail--client #'julia-snail--server-response-filter))))))


(defun julia-snail--disable ()
  (julia-snail--cleanup))


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
      (sleep-for 0 20)
      ;; wait for the inclusion to succeed (i.e., the prompt prints)
      (let ((sleep-total 0))
        (while (and (< sleep-total 5000)
                    (not (string-equal "julia>" (current-word))))
          (sleep-for 0 20)
          (setf sleep-total (+ sleep-total 20)))))))


(defun julia-snail--send-to-server (repl-buf str)
  "Send str to Snail server."
  (declare (indent defun))
  (unless repl-buf
    (error "no REPL buffer given"))
  (let* ((client-buf (get-buffer (julia-snail--client-buffer-name repl-buf)))
         ;; FIXME: Support other namespaces!
         (reqid (format "%04x%04x" (random (expt 16 4)) (random (expt 16 4))))
         (msg (format "(ns = [:Main], reqid = \"%s\", code = %s)\n"
                      reqid
                      (json-encode-string str))))
    (with-current-buffer client-buf
      (goto-char (point-max))
      (insert msg))
    (process-send-string client-buf msg)
    (puthash reqid :nothing julia-snail--requests)
    reqid))


(defun julia-snail--send-to-server-via-tmp-file (repl-buf str)
  "Send str to server by first writing it to a tmpfile, calling
Julia include on the tmpfile, and then deleting the file."
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
                     repl-buf (format "include(\"%s\");" tmpfile))))
        (puthash reqid tmpfile julia-snail--requests)))))


(defun julia-snail--server-response-filter (proc str)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; insert at the end unconditionally
      (goto-char (point-max))
      (insert str)
      (set-marker (process-mark proc) (point))
      ;; scary
      (eval (read str)))))


 ;;; --- Snail server response handling functions

(defun julia-snail--response-base (reqid)
  (let ((request-info (gethash reqid julia-snail--requests)))
    (when request-info
      ;; tmpfile
      (when (stringp request-info)
        (delete-file request-info))
      ;; remove request ID from requests hash
      (remhash reqid julia-snail--requests))))


(defun julia-snail--response-done (reqid)
  (julia-snail--response-base reqid))


(defun julia-snail--response-error (reqid error-message error-stack)
  (julia-snail--response-base reqid)
  ;; FIXME: Make this much nicer.
  (message (format "something broke: %s" error-message)))


;;; --- commands

(defun julia-snail ()
  "FIXME: Write this."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (if repl-buf
        (pop-to-buffer-same-window repl-buf)
      ;; run Julia in a vterm and load the Snail server file
      (let ((vterm-shell (format "%s -L %s" julia-snail-executable julia-snail--server-file)))
        (vterm julia-snail-repl-buffer)
        (julia-snail-mode)))))


(defun julia-snail-send-line ()
  "FIXME: Write this."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (when repl-buf
      (let ((line (s-trim (thing-at-point 'line t))))
        (julia-snail--send-to-repl repl-buf
          line)))))


(defun julia-snail-send-buffer ()
  "FIXME: Write this."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer))
        (filename buffer-file-name))
    (when repl-buf
      (julia-snail--send-to-server repl-buf
        (format "include(\"%s\");" filename)))))


(defun julia-snail-send-region ()
  "FIXME: Write this."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (when (and repl-buf (use-region-p))
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (julia-snail--send-to-server-via-tmp-file repl-buf text)))))


(defun julia-snail-send-top-level-form ()
  "FIXME: Write this."
  (interactive)
  ;; FIXME: Convert to sending by socket.
  ;; TODO: Use a Julia parser and support module context.
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (when repl-buf
      (let ((form (s-trim (thing-at-point 'defun t))))
        (julia-snail--send-to-repl repl-buf
          form)))))


;;; --- mode definition

;;;###autoload
(define-minor-mode julia-snail-mode
  "A minor mode for interactive Julia development. Should only be
turned on in REPL buffers."
  :init-value nil
  :lighter " Snail"
  :keymap (make-sparse-keymap)
  (if julia-snail-mode
      (julia-snail--enable)
    (julia-snail--disable)))

(provide 'julia-snail-mode)
