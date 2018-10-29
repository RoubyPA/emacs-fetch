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
  (test "ef-cut-line" 42)
  ;; ef-display
  (test "ef-display" 42)
  ;; ef-emacs-info
  (test "ef-emacs-info" 42)
  ;; ef-uname
  (test "ef-uname" 42)
  ;; ef-cpu-model
  (test "ef-cpu-model" 42)
  ;; ef-gpu-model
  (test "ef-gpu-model" 42)
  ;; ef-computer
  (test "ef-computer" 42)
  ;; ef-uptime
  (test "ef-uptime" 42)
  ;; ef-load-avg
  (test "ef-load-avg" 42)
  ;; ef-shell
  (test "ef-shell" 42)
  ;; ef-desktop
  (test "ef-desktop" 42)
  ;; ef-resolution-select-line
  (test "ef-resolution-select-line" 42)
  ;; ef-resolution-format-string
  (test "ef-resolution-format-string" 42)
  ;; ef-resolution
  (test "ef-resolution" 42)
  ;; ef-distro
  (test "ef-distro" 42)
  ;; ef-installed-package
  (test "ef-installed-package" 42)
  ;; ef-login-host
  (test "ef-login-host" 42)


  ;; covered test
  (message "(covered %2.2f%%)"
           (* 100 (/ tests-passed tests-total))))

(provide 'efetch-tests)
;;; efetch-tests.el ends here