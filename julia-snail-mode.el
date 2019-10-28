;;; julia-snail-mode.el --- Julia Snail -*- lexical-binding: t -*-


;;; ---

(defgroup julia-snail nil
  "Customization options for Julia Snail mode."
  :group 'external)


(defcustom julia-snail-executable "julia"
  "FIXME: Write this."
  :tag "Julia executable to run"
  :group 'julia-snail
  :type 'string)
;;(make-variable-buffer-local 'julia-snail-executable)


(defcustom julia-snail-repl-buffer "*julia-repl*"
  "FIXME: Write this."
  :tag "Name of buffer to use for REPL interaction"
  :group 'julia-snail
  :type 'string)
;;(make-variable-buffer-local 'julia-snail-executable)


;;; ---

(define-minor-mode julia-snail-mode
  "A minor mode for interactive Julia development."
  :init-value nil
  :lighter " Snail"
  :keymap (make-sparse-keymap))


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
        (save-excursion
          (with-current-buffer repl-buf
            (vterm-send-string line)
            (vterm-send-return)))))))


(defun julia-snail-send-buffer ()
  "FIXME: Write this."
  (interactive)
  (let ((repl-buf (get-buffer julia-snail-repl-buffer))
        (filename buffer-file-name))
    (when repl-buf
      (save-excursion
        (with-current-buffer repl-buf
          (vterm-send-string (format "include(\"%s\");" filename))
          (vterm-send-return))))))


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
          (save-excursion
            (with-current-buffer buf
              (vterm-send-string (format "include(\"%s\");" tmpfile))
              (vterm-send-return)
              (sleep-for 0 20)
              ;; wait for the inclusion to succeed (i.e., the prompt prints)
              (let ((sleep-total 0))
                (while (and (< sleep-total 5000)
                            (not (string-equal "julia>" (current-word))))
                  (sleep-for 0 20)
                  (setf sleep-total (+ sleep-total 20)))))))
      ;; cleanup
      (delete-file tmpfile))))


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


;;; ---

(provide 'julia-snail-mode)
