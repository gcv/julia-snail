;;; repl-history.el --- Julia Snail -*- lexical-binding: t -*-


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A Julia Snail extension to access Julia REPL history from julia-snail-mode buffers.
;; - julia-snail/repl-history-search-and-yank provides a search interface
;; - julia-snail/repl-history-yank yanks recent history entries (1 by default)
;; - julia-snail/repl-history-buffer opens a buffer with the history

;;; Code:


;;; --- requirements

(require 'julia-snail)


;;; --- customizations

(defgroup julia-snail/repl-history nil
  "Customization options for the REPL history plugin."
  :group 'julia-snail)

(defcustom julia-snail/repl-history-default-size 10000
  "How many entries of REPL command history to retrieve by default."
  :tag "Number of Julia REPL history entries"
  :group 'julia-snail/repl-history
  :safe 'integerp
  :type 'integer)


;;; --- initialiation function

(defun julia-snail/repl-history-init (repl-buf)
  (julia-snail--send-to-server
    '("JuliaSnail" "Extensions")
    "load([\"repl-history\" \"REPLHistory.jl\"]); REPLHistory.init()"
    :repl-buf repl-buf
    :async nil))


;;; --- implementation

(defun julia-snail/repl-history--buffer-name ()
  (concat julia-snail-repl-buffer " REPL command history"))

(cl-defun julia-snail/repl-history--fetch (n)
  (julia-snail--send-to-server
    '("JuliaSnail" "Extensions" "REPLHistory")
    (format "replhistory(%i)" n)
    :async nil))

(cl-defun julia-snail/repl-history-yank (&optional (n 1))
  "Paste last N lines from Julia REPL history into current buffer.

By default n=1, but the value can be given as a prefix argument."
  (interactive "p")
  (let ((res (julia-snail/repl-history--fetch n)))
    (insert (string-join res "\n"))))

(cl-defun julia-snail/repl-history-search-and-yank (&optional (n julia-snail/repl-history-default-size))
  "Search Julia REPL history and insert hit at point.
This uses completing-read, so the search interface can be provided by Ivy/Counsel/Helm etc.
Also works in the REPL, where it can substitue for Ctrl+R. A
limitation is that only the lines in the main Julia mode can be
searched (excluding shell mode or package mode, for instance).
Optional argument N sets the maximum number of lines of history
to search through."
  (interactive)
  (let* ((hst (julia-snail/repl-history--fetch n))
         (res (completing-read "Julia REPL history search: " hst)))
    (if (symbol-value julia-snail-repl-mode)
        (vterm-insert res)
      (insert res))))

(cl-defun julia-snail/repl-history-buffer (&optional (n julia-snail/repl-history-default-size))
  "Display last N lines of Julia REPL history in a separate buffer."
  (interactive)
  (let ((buf (get-buffer-create (julia-snail/repl-history--buffer-name)))
        (hst (julia-snail/repl-history--fetch n)))
    (julia-snail--add-to-perspective buf)
    (with-current-buffer buf
      (erase-buffer)
      (insert (string-join hst "\n"))
      (goto-char (point-min))
      (julia-mode)
      (julia-snail-mode)
      (pop-to-buffer buf))))


;;; --- extension minor-mode

(defvar julia-snail/repl-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c j r h C-y") #'julia-snail/repl-history-yank)
    (define-key map (kbd "C-c j r h C-s") #'julia-snail/repl-history-search-and-yank)
    (define-key map (kbd "C-c j r h C-o") #'julia-snail/repl-history-buffer)
    map))

(define-minor-mode julia-snail/repl-history-mode
  "Julia Snail extension: REPL history."
  :init-value nil
  :lighter ""
  :keymap julia-snail/repl-history-mode-map)


;;; --- done

(provide 'julia-snail/repl-history)


;;; repl-history.el ends here
