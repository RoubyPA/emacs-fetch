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

(setq efetch-highlights
      '(("\\(.*\\) :" . font-lock-function-name-face)
        ("Efetch"     . font-lock-constant-face)))

(define-derived-mode efetch-mode fundamental-mode "efetch"
  "Major mode for displaying efetch buffer."
  (setq font-lock-defaults '(efetch-highlights)))

(provide 'efetch-mode)
;;; efetch-mode.el ends here
