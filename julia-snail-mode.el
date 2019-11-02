;;; julia-snail-mode.el --- Julia Snail -*- lexical-binding: t -*-


;;; --- requirements

(require 'cl-lib)
(require 's)
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

(defvar julia-snail--server-file (concat default-directory "JuliaSnail.jl"))


;;; --- supporting functions

(defun julia-snail--client-buffer-name (repl-buf)
  (let ((real-buf (get-buffer repl-buf)))
    (unless real-buf
      (error "no REPL buffer found"))
    (format "%s client" (buffer-name (get-buffer real-buf)))))


(defun julia-snail--cleanup ()
  (let ((client-buf (get-buffer (julia-snail--client-buffer-name (current-buffer)))))
    (when client-buf
      (kill-buffer client-buf)))
  (setq julia-snail--client nil))


(cl-defun julia-snail--send-to-repl (repl-buf str &key (async t))
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


(defun julia-snail--send-via-tmp-file (buf text-raw)
  (let ((text (s-trim text-raw))
        (tmpfile (make-temp-file
                  (expand-file-name "julia-tmp"
                                    (or small-temporary-file-directory
                                        temporary-file-directory)))))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert text))
          (julia-snail--send-to-repl buf
            (format "include(\"%s\");" tmpfile)
            :async nil))
      ;; cleanup
      (delete-file tmpfile))))


(defun julia-snail--enable ()
  (add-hook 'kill-buffer-hook #'julia-snail--cleanup nil t)
  (let ((repl-buf (current-buffer))
        (client-buf (get-buffer-create (julia-snail--client-buffer-name (current-buffer)))))
    (when (fboundp #'persp-add-buffer) ; perspective-el support
      (persp-add-buffer client-buf))
    (with-current-buffer client-buf
      (unless julia-snail--client
        (julia-snail--send-to-repl repl-buf
          (format "include(\"%s\");" julia-snail--server-file))
        (julia-snail--send-to-repl repl-buf
          (format "JuliaSnail.start(%d);" julia-snail-port)
          :async nil)
        (with-current-buffer repl-buf
          (setq julia-snail--client ; NB: buffer-local variable!
                (open-network-stream "julia-client" client-buf "localhost" julia-snail-port)))))))


(defun julia-snail--disable ()
  (julia-snail--cleanup))


;;; --- commands

(defun julia-snail ()
  "FIXME: Write this."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (if repl-buf
        (pop-to-buffer-same-window repl-buf)
      (let ((vterm-shell julia-snail-executable))
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
      (julia-snail--send-to-repl repl-buf
        (format "include(\"%s\");" filename)))))


(defun julia-snail-send-region ()
  "FIXME: Write this."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (when (and repl-buf (use-region-p))
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (julia-snail--send-via-tmp-file repl-buf text)))))


(defun julia-snail-send-top-level-form ()
  "FIXME: Write this."
  (interactive)
  (let* ((repl-buf (get-buffer julia-snail-repl-buffer)))
    (when repl-buf
      (let ((starting-point (point)))
        (save-excursion
          (beginning-of-line)
          (unless (= 1 starting-point) (left-char))
          (let* ((ending-raw (re-search-forward "\nend\n\n" nil t))
                 ;; subtract two trailing newlines
                 (ending (- (or ending-raw 0) 2))
                 (beginning-raw (re-search-backward
                                 (concat "\\(\nfunction\\)\\|"
                                         "\\(\ntype\\)")))
                 ;; add one leading newline
                 (beginning (+ beginning-raw 1)))
            (if (or (not (and ending-raw beginning-raw))
                    (< starting-point beginning)
                    (> starting-point ending))
                (message "no suitable top-level form found")
              (let ((text (buffer-substring-no-properties beginning ending)))
                (julia-snail--send-via-tmp-file repl-buf text)))))))))


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
