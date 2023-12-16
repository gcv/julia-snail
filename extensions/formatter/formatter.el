;;; formatter.el --- Julia Snail -*- lexical-binding: t -*-


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

;; A Julia Snail extension to format regions and buffers using JuliaFormatter.jl
;; (https://github.com/domluna/JuliaFormatter.jl). Inspired by
;; https://codeberg.org/FelipeLema/julia-formatter.el.

;;; Code:


;;; --- requirements

(require 'julia-snail)


;;; --- initialiation function

(defun julia-snail/formatter-init (repl-buf)
  (julia-snail--send-to-server
    '("JuliaSnail" "Extensions")
    "load([\"formatter\" \"Formatter.jl\"]); Formatter.init()"
    :repl-buf repl-buf
    :async nil
    :async-poll-maximum 120000))


;;; --- implementation

(defun julia-snail/formatter--format-text (txt)
  (julia-snail--send-to-server
    '("JuliaSnail" "Extensions" "Formatter")
    (let* ((ubs (string-as-unibyte txt))
           (estr (base64-encode-string ubs))
           (pathstr (base64-encode-string (buffer-file-name))))
      (format "format_data(\"%s\", \"%s\")" estr  pathstr))
    :async nil))


(defun julia-snail/formatter-format-region (begin end)
  "Format region delimited by BEGIN and END using JuliaFormatter.jl.
The code in the region must be syntactically valid Julia, otherwise no formatting will take place."
  (interactive "r")
  (let* ((text-to-be-formatted (buffer-substring-no-properties begin end))
         (ftext (julia-snail/formatter--format-text text-to-be-formatted)))
    (if (eq :nothing ftext)
        (message "Parsing error, formatting failed")
      (delete-region begin end)
      (insert ftext))))


(defun julia-snail/formatter-format-buffer ()
  "Format buffer using JuliaFormatter.jl.
The buffer must be syntactically valid Julia, otherwise no formatting will take place.
Point placement after reformatting is sketchy, since the code might have changed quite a bit."
  (interactive)
  (let* ((old-point (point)))
    (julia-snail/formatter-format-region (point-min) (point-max))
    (goto-char old-point)))


;;; --- extension minor-mode

(defvar julia-snail/formatter-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c j f r") #'julia-snail/formatter-format-region)
    (define-key map (kbd "C-c j f b") #'julia-snail/formatter-format-buffer)
    map))

(define-minor-mode julia-snail/formatter-mode
  "Julia Snail extension: JuliaFormatter.jl integration."
  :init-value nil
  :lighter ""
  :keymap julia-snail/formatter-mode-map)


;;; --- done

(provide 'julia-snail/formatter)


;;; formatter.el ends here
