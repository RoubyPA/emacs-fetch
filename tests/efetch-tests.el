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

;; Test 'efetch.el' functions. Lot of function could no be tested,
;; because depend of system, current software environment, ...

;;; Code:

(defconst test-format "%s:\t%s"
  "Format of tests result, for example 'test1: PASS'.")

(defconst test-pass "PASSED")
(defconst test-fail "FAILED")
(defconst test-todo "TODO")

(defvar tests-total 0.0)
(defvar tests-passed 0.0)
(defvar tests-failed 0.0)

(defun test (name res)
  (setq tests-total (+ tests-total 1))
  (cond
   ;; PASS
   ((eq res t)
    (setq tests-passed (+ tests-passed 1))
    (message test-format name test-pass))
   ;; FAIL
   ((eq res nil)
    (setq tests-failed (+ tests-failed 1))
    (message test-format name test-fail))
   ;; ELSE
   (t
    (message test-format name test-todo))))

(defun run ()
  ;; ef-get-first-line
  (test "ef-get-first-line" (equal "foo" (ef-get-first-line "foo\nbar\n")))
  ;; ef-add-spaces
  (test "ef-add-spaces" (equal "foo*****" (ef-add-spaces "foo" 5 "*")))
  ;; ef-cut-line
  ;; (test "ef-cut-line" 42) ;Could not be tested
  ;; ef-display
  ;; (test "ef-display" 42) ;Could not be tested
  ;; ef-emacs-info
  (test "ef-emacs-info" (not (equal "" (ef-emacs-info))))
  ;; ef-uname
  (test "ef-uname -r" (not (equal "" (ef-uname "-r"))))
  (test "ef-uname -s" (not (equal "" (ef-uname "-s"))))
  ;; ef-cpu-model
  (test "ef-cpu-model" (not (equal "" (ef-cpu-model))))
  ;; ef-gpu-model
  (test "ef-gpu-model" (not (equal "" (ef-gpu-model))))
  ;; ef-computer
  (test "ef-computer" (not (equal "" (ef-computer))))
  ;; ef-uptime
  (test "ef-uptime" (not (equal "" (ef-uptime))))
  ;; ef-load-avg
  (test "ef-load-avg" (not (equal "" (ef-load-avg))))
  ;; ef-shell
  (test "ef-shell" (not (equal "" (ef-shell))))
  ;; ef-desktop
  (test "ef-desktop" (not (equal "" (ef-desktop))))
  ;; ef-resolution-select-line
  (test "ef-resolution-select-line-01"
	(equal
	 nil
	 (ef-resolution-select-line
	  "Screen 0: minimum 320 x 200, current 2880 x 1024, maximum 8192 x 8192")))
  (test "ef-resolution-select-line-02"
	(equal
	 nil
	 (ef-resolution-select-line
	  "   1280x800      59.99    59.97    59.81    59.91 ")))
  (test "ef-resolution-select-line-03"
	(not
	 (equal
	  nil
	  (ef-resolution-select-line
	   "   1600x900      60.01*+  59.99    59.94    59.95    59.82    40.00"))))
  ;; ef-resolution-format-string
  ;; (test "ef-resolution-format-string" 42)
  ;; ef-resolution
  (test "ef-resolution" 42)
  ;; ef-distro
  (test "ef-distro" (not (equal "Unknown OS" (ef-distro))))
  ;; ef-installed-package
  (test "ef-installed-package"
	(not (equal "" (ef-installed-package (ef-distro)))))

  ;; covered test
  (message "(covered %2.2f%%)"
           (* 100 (/ tests-passed tests-total))))

(provide 'efetch-tests)
;;; efetch-tests.el ends here
