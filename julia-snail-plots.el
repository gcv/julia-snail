(define-minor-mode julia-snail-plot-mode
  "A minor mode for displaying a Julia plot inside an Emacs buffer."
  :init-value nil
  :lighter "Julia Plot"
  :keymap '(((kbd "q") . julia-snail--quit-plot-window)))


(defcustom julia-snail-pop-plot nil
  "If true, when a plot is displayed inside Emacs, the plot buffer gets the focus (e.g., for zooming and panning). Hit q to return to REPL. If nil, the plot window is displayed but focus remains on the REPL buffer."
  :tag "Julia plotting"
  :group 'julia-snail
  :type 'boolean)  


;; entry point from Julia - modify here to support different display modes
(defun julia-snail--draw-plot (im encoded)
  (julia-snail--show-im im encoded))

(defun julia-snail-toggle-plotting-in-emacs ()
"Turn on/off plotting in emacs. This calls 'JuliaSnail.toggle_display()', which pushes/pops an Emacs display onto Julia's display stack"
(interactive)
(let ( (repl-buf  (get-buffer julia-snail-repl-buffer)))
  (server-start)
  (message (julia-snail--send-to-server
     :JuliaSnail
     "toggle_display()"
     :repl-buf repl-buf
     :async nil
    ))))


;; Insert "im" (a string) into the buffer, decode if necessary, and call image-mode
(defun julia-snail--show-im (im decode)
  (interactive)
  (let ((buf (get-buffer-create "*julia plots*")))
    (with-current-buffer buf
      (fundamental-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert im)
      (if decode (base64-decode-region (point-min) (point-max)))


      (if julia-snail-pop-plot (pop-to-buffer buf)
        (display-buffer buf))
      (image-mode)
      (julia-snail-plot-mode)
      (goto-char (point-min))
      )))

(defun julia-snail--quit-plot-window ()
  (interactive)
  (progn
    (with-current-buffer julia-snail-repl-buffer
      (goto-char (point-max))) ; required, point is sometimes a few lines back
    (pop-to-buffer julia-snail-repl-buffer)
    ))





;; Another possible mode for plot display: insert successively in a single buffer. I'm disabling this for now, it's not very useful in the current form

;; (defcustom julia-snail-single-plot t
;;   "If true, plots are showed one-by-one. If false, plots are inserted successively in the plotting buffer"
;;   :tag "Julia plotting"
;;   :group 'julia-snail
;;   :type 'boolean)  

;; (defun julia-snail--insert-im (im decode)
;;   (interactive)
;;   (progn
;;     (if decode (setq im (base64-decode-string im)))

;;     (let ((buf (get-buffer-create "*julia plots*")) (img (create-image im nil t)))
;;     (with-current-buffer buf
;;       (goto-char (point-max))
;;       (unless  (= (point-max) (point-min))
;;         (progn
;;           (insert (propertize "      \n" 'face 'underline))
;;           (insert "\n")
;;           ))
;;       (insert-image img "julia plot")
;;       (insert-char ?\n 2)
;;       (pop-to-buffer buf)
;;       )
;;     ))
;;   )
;; demo code for displaying a stack trace in its own Emacs window
;; not plotting per se, disabled
;; (defun julia-snail--show-trace (trace)
;;   (let* ((repl-buf (get-buffer julia-snail-repl-buffer))
;;          (msg (format "%s\n\n%s" "Stacktrace:" (s-join "\n" trace)))
;;          (error-buffer (julia-snail--message-buffer repl-buf "Julia error" msg))
;;          (process-buf (get-buffer (julia-snail--process-buffer-name repl-buf)))
;;          )
;;     (julia-snail--setup-compilation-mode error-buffer (gethash process-buf julia-snail--cache-proc-basedir))
;;     (pop-to-buffer error-buffer)
;;   ))



(provide 'julia-snail-plots)

