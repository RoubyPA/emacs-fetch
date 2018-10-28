;;; efetch-tests.el --- Efetch tests.                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Pierre-Antoine Rouby

;; Author: Pierre-Antoine Rouby <contact@parouby.fr>
;; Keywords: extensions, lisp

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

;; Test 'efetch.el' functions.

;;; Code:

(defconst test-format "%s: %s"
  "Format of tests result, for example 'test1: PASS'.")

(defconst test-pass "PASS")
(defconst test-fail "FAILED")

(defvar run-tests t)

(defun test (name res)
  (if res
      (message test-format name test-pass)
    (message test-format name test-fail)))

(defun run ()
  (test "pass" t))

(provide 'efetch-tests)
;;; efetch-tests.el ends here
