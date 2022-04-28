
;;; --- requirements

(require 'julia-snail)
(require 'popup)


;;; --- initialisation function



(defun julia-snail/snail-popup-init (repl-buf)
  (julia-snail--send-to-server
    '("JuliaSnail" "Extensions")
    "load([\"snail-popup\" \"Popup.jl\"]); Popup.init()"
    :repl-buf repl-buf
    :async nil
    :async-poll-maximum 120000))


;;; --- implementation


(defun julia-snail/send-line-and-popup ()
  "Evaluate the line at point and display its value as a popup."
  (interactive)
  (progn
    (julia-snail-send-line)
    (let* ((err (julia-snail--send-to-server
                :Main
                "Base.active_repl.waserror"
                ;; line didn't run
                :async nil))
         (str (if (equal err :nothing) (julia-snail--send-to-server
                              :Main
                              "JuliaSnail.Extensions.Popup.display_obj(ans)"
                              :async nil) "error" ))
         )
      (popup-tip str))))




;;; --- extension minor-mode

(defvar julia-snail/popup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v v") #'julia-snail/send-line-and-popup)
    map))

(define-minor-mode julia-snail/snail-popup-mode
  "Julia Snail extension: show value in popup."
  :init-value nil
  :lighter ""
  :keymap julia-snail/popup-mode-map)


;;; --- done

(provide 'julia-snail/snail-popup)


;;; snail-popup.el ends here
