

;; Some commands for querying the Julia REPL history from emacs
;; To use, set eg.
;; (define-key julia-snail-repl-mode-map (kbd "C-c C-s") #'julia-snail-search-history-and-insert)
;; (define-key julia-snail-mode-map (kbd "C-c h i") #'julia-snail-insert-history)
;; (define-key julia-snail-mode-map (kbd "C-c h o") #'julia-snail-history-occur)
;; (define-key julia-snail-mode-map (kbd "C-c h s") #'julia-snail-view-session-history)

(require 'rx)
(require 'ivy)
(require 'swiper)
(defun julia-snail--find-history-file ()
  (julia-snail--send-to-server
  nil
  "import REPL; REPL.find_hist_file()"
  :async nil))

(defvar-local julia-snail-history-buf "*julia* REPL command history")

(defun julia-snail-history-occur (pattern)
  (interactive "sSearch history for: ")
  (progn
    (julia-snail--load-history)
    (with-current-buffer (get-buffer julia-snail-history-buf)
      (occur pattern)
    )))

(defcustom julia-snail-max-history-lines 10000
  "How many lines of REPL command history to display ."
  :tag "Max. number of Julia REPL history lines"
  :group 'julia-snail
  :safe 'integerp
  :type 'integer)




(defun julia-snail--load-history (&optional nlines)
  "Load nlines of Julia REPL history. If nlines is missing, it is set to julia-snail-max-history-lines"
  (interactive "p")
  (progn
    (unless nlines
      (setq nlines julia-snail-max-history-lines))

    ;; (let ((a 10))
    ;;   (forward-line (- nlines))
    ;;   )))
    (when (get-buffer julia-snail-history-buf )
      (kill-buffer julia-snail-history-buf))
    (let ((buf (get-buffer-create julia-snail-history-buf)) (nl nlines))
      (with-current-buffer buf
        (insert-file-contents (julia-snail--find-history-file))
        (delete-matching-lines "^#")
        (while (re-search-forward (rx bol (one-or-more blank)) nil t)
          (replace-match "" nil nil))
        (goto-char (point-max))

        (forward-line (- nl))
        (read-only-mode)
        (goto-char (point-max))
        ))
    )
  )



(defun julia-snail--search-history (&optional nlines)
  (interactive "p")
  (progn
    (julia-snail--load-history)
    (let ((line (with-current-buffer julia-snail-history-buf
                  (let ((candidates (swiper--candidates)))
                    (ivy-read
                     "History: "
                     (seq-reverse candidates)        )
                    )))
          )
      (setq line (substring-no-properties line 1))
      line
      )
    ))

(defun julia-snail-view-session-history ()
  "Display session history in a separate buffer"
  (interactive)
  (julia-snail--load-history)
  (let ((buf (get-buffer julia-snail-history-buf)))
  (with-current-buffer buf
    (widen)
    (goto-char (point-max))
    (search-backward "JuliaSnail.start")
    (forward-line)
    (narrow-to-region (point) (point-max))
    (pop-to-buffer buf)
    ))
  )

(defun julia-snail-insert-history (&optional nlines)
  "Insert nlines of REPL history into buffer. If nlines is missing, insert just the latest entry"
  (interactive "p")
  (setq nlines (if nlines nlines 1 ))
  (julia-snail--load-history)
  (let ((buf (get-buffer julia-snail-history-buf)))
    (with-current-buffer buf
      (widen)
      (goto-char (point-max))
      (forward-line (- nlines))
      (narrow-to-region (point) (point-max)))
    (insert-buffer-substring buf)
    (with-current-buffer buf (widen))
    ))


(defun julia-snail-search-history-and-insert ()
  (interactive)
  "Use ivy to search REPL history and insert result at prompt"
  (let ( (line (julia-snail--search-history))
         (repl-buf (get-buffer julia-snail-repl-buffer)))
    (pop-to-buffer repl-buf)
    (with-current-buffer repl-buf
      (vterm-send-string line)
      )))


(provide 'julia-snail-history)
