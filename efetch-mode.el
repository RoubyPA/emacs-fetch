;;; efetch-mode.el --- Efetch major mode.            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Pierre-Antoine Rouby

;; Author: Pierre-Antoine Rouby <contact@parouby.fr>
;; Keywords: lisp, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for Efetch buffer.

;;; Code:

;;; Highlights
(setq efetch-highlights
      '(("\\(.*\\) :" . font-lock-function-name-face)
        ("Efetch"     . font-lock-constant-face)))

;;; Keymap
(defvar efetch-mode-map nil "Keymap for `efetch-mode-map'")
(setq efetch-mode-map (make-sparse-keymap))
(define-key efetch-mode-map (kbd "C-c C-c") 'efetch-update)

;;; Define mode
(define-derived-mode efetch-mode fundamental-mode "efetch"
  "Major mode for displaying efetch buffer."
  (setq font-lock-defaults '(efetch-highlights))
  (use-local-map efetch-mode-map))

(provide 'efetch-mode)
;;; efetch-mode.el ends here
