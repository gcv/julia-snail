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


(defun julia-snail--send-to-repl-via-tmp-file (repl-buf text-raw)
  (unless repl-buf
    (error "no REPL buffer given"))
  (let ((text (s-trim text-raw))
        (tmpfile (make-temp-file
                  (expand-file-name "julia-tmp"
                                    (or small-temporary-file-directory
                                        temporary-file-directory)))))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert text))
          (julia-snail--send-to-repl repl-buf
            (format "include(\"%s\");" tmpfile)
            :async nil))
      ;; cleanup
      (delete-file tmpfile))))


(defun julia-snail--send-to-server (repl-buf str)
  (declare (indent defun))
  (unless repl-buf
    (error "no REPL buffer given"))
  (let ((client-buf (get-buffer (julia-snail--client-buffer-name repl-buf)))
        (msg (format "(ns = [:Main], code = %s)\n" (json-encode-string str))))
    (process-send-string client-buf msg)))


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
        (julia-snail--send-to-repl-via-tmp-file repl-buf text)))))


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
