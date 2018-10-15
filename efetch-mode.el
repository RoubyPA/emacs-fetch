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

(defvar ef-images-dir "~/.emacs.d/efetch-mode/images/")
(defvar ef-default-spaces 15)
(defvar ef-default-separator " : ")
(defvar ef-distro-image '(("GuixSD" . "guix.png")
                          (t . "default.png")))

(defun ef-add-spaces (str n)
  (if (= n 0)
      str
    (ef-add-spaces (concat str " ") (- n 1))))

(defun ef-display (l)
  "Display new line in eftech buffer."
  (insert
   (concat (ef-add-spaces (car l) (- ef-default-spaces (length (car l))))
           ef-default-separator (cdr l) "\n")))

(defun ef-emacs-info ()
  (let ((emacs-info (split-string (emacs-version))))
    (concat (nth 0 emacs-info) " "
            (nth 1 emacs-info) " "
            (nth 2 emacs-info))))

(defun ef-uname (opt)
  "Exec uname with 'opt' and return the first line of command
output."
  (car (split-string
        (shell-command-to-string (format "uname %s" opt))
        "\n")))

(defun ef-cpu-model ()
  (let ((cmd "cat /proc/cpuinfo |grep 'model name'"))
    (car (cdr (split-string
               (car (split-string (shell-command-to-string cmd)
                                  "\n"))
               ": ")))))

(defun ef-uptime ()
  (nth 2 (split-string (shell-command-to-string "uptime"))))

(defun ef-load-avg ()
  (with-temp-buffer
    (insert-file-contents "/proc/loadavg")
    (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))
           (sline (split-string line " ")))
      (concat (nth 0 sline) " "
              (nth 1 sline) " "
              (nth 2 sline)))))

(defun ef-shell ()
  "Return shell name."
  (capitalize (file-name-nondirectory (getenv "SHELL"))))

(defun ef-distro ()
  "Return operation system name and version."
  (cond
   ;; lsb_release
   ((eq (shell-command "type -p lsb_release") 0)
    (shell-command-to-string "lsb_release -sd"))
   ;; Guix
   ((eq (shell-command "type -p guix") 0)
    (concat "GuixSD "
            (nth 4 (split-string
                    (shell-command-to-string
                     "guix system -V")
                    " \\|\n"))))
   ;; Default
   (t "Unknown OS")))

(defun ef-insert-os-image (os)
  "Insert image corresponding to OS distribution. If OS don't
have images, use default images."
  (let* ((distro (car (split-string os " ")))
         (search (assoc distro ef-distro-image))
         (image  (if search
                     (cdr search)
                   (cdr (assoc t ef-distro-image)))))
    (insert-image
     (create-image (concat ef-images-dir image)))))

(defun efetch-write-data ()
  (let ((data  `(("OS"     . ,(ef-distro))
                 ("Host"   . ,(system-name))
                 ("Uptime" . ,(ef-uptime))
                 ("Kernel" . ,(ef-uname "-s"))
                 ("Kernel version" . ,(ef-uname "-r"))
                 ("Arch"   . ,(ef-uname "-m"))
                 ("CPU"    . ,(ef-cpu-model))
                 ("Load average" . ,(ef-load-avg))
                 ("Shell"  . ,(ef-shell))
                 ("Emacs"  . ,(ef-emacs-info))
                 ("Emacs uptime" . ,(emacs-uptime)))))
    ;; Insert Header
    (insert "+++ Efetch +++\n")
    (ef-insert-os-image (ef-distro))
    (insert "\n")                       ;New line after image

    ;; Insert data
    (mapc 'ef-display data)))

;;;###autoload
(defun efetch ()
  "Display emacs and system configuration in new buffer."
  (interactive)
  (let* ((buff  (generate-new-buffer "efetch")))
    ;; Switch to new buffer and active efetch-mode
    (set-buffer buff)
    (switch-to-buffer buff)
    (efetch-mode)

    ;; Insert data
    (efetch-write-data)

    ;; Set buffer to read only mode
    (read-only-mode t)))

(defun efetch-update ()
  "Update current efetch buffer."
  (interactive)
  ;; Set buffer writable
  (if (not (string-equal mode-name "efetch"))
      (progn (message "efetch: Not valide buffer !")
             nil)
    (progn
      (read-only-mode 0)
      (erase-buffer)
      ;; Insert data
      (efetch-write-data)
      ;; Set buffer to read only mode
      (read-only-mode t))))

;;; Highlights
(defvar efetch-highlights
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

(provide 'efetch)
;;; efetch-mode.el ends here
