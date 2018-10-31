;;; efetch.el --- Efetch major mode.                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Pierre-Antoine Rouby
;; Copyright (C) 2018  David Tabarie

;; Author: Pierre-Antoine Rouby <contact@parouby.fr>
;;         David Tabarie <david.tabarie@gmail.com>
;; Version: 0.0.4
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

;; Efetch provide an interface for visualising system information in
;; emacs.

;;; Code:

(defconst efetch-version "0.0.4"
  "Current efetch version.")

(defvar ef-dir (file-name-directory load-file-name)
  "Load file dir.")

(defvar ef-images-dir
  (let ((dir (concat ef-dir "images/")))
    (if (file-exists-p (concat dir "default.png"))
        dir
      ef-dir))
  "Efetch images directory.")

(defvar ef-ascii-dir
  (let ((dir (concat ef-dir "ascii-arts/")))
    (if (file-exists-p (concat dir "default"))
        dir
      ef-dir))
  "Efetch ascii-arts directory.")

(defvar ef-margin 15
  "Eftech margin between start of line and data.")

(defvar ef-separator " : "
  "Efetch separator in string.")

(defvar ef-custom-image ""
  "Path of custom image to replace distro image. By default this
variable is empty.")

(defvar ef-custom-ascii ""
  "Path of custom ascii to replace distro image. By default this
variable is empty.")

(defvar ef-additional-data '()
  "Additional data list for efetch buffer.

Data list is a list of associated list, like:
  '((\"name\"  . \"value\")
    (\"name2\" . \"value2)")

(defvar ef-fast-display nil
  "When is t disable time expensive function.")

(defvar graphic-session (display-graphic-p)
  "Return t when emacs is run in graphic mode.") ;

(defun ef-get-first-line (str)
  "Return first line of STR."
  (car (split-string str "\n")))

(defun ef-add-spaces (str n &optional sep)
  "Add N spaces to STR. You can replace spaces by SEP."
  (when (equal sep nil)
    (setq sep " "))
  (if (= n 0)
      str
    (ef-add-spaces (concat str sep)
                   (- n 1) sep)))

(defun ef-cut-line (str)
  "If STR length if to long for display, cut the line and add
'...'  as suffix and return this new string, else return STR."
  (if (> (+ ef-margin (length str) (length ef-separator))
         (window-width))
      (concat (substring str 0
                         (- (window-width)
                            ef-margin
                            (length ef-separator)
                            ;; Length of "..." + 1
                            4))
              "...")
    str))

(defun ef-display (l)
  "Display new line in eftech buffer.

L is a list like (\"key\" . \"value\")."
  (unless (or (equal l '())
              (equal l nil))
    (insert (car l))
    (indent-to ef-margin)
    (insert ef-separator (ef-cut-line (cdr l))
            "\n")))

(defun ef-emacs-info ()
  "Return emacs informations as a string."
  (let ((emacs-info (split-string (emacs-version))))
    (concat (nth 0 emacs-info) " "
            (nth 1 emacs-info) " "
            (nth 2 emacs-info))))

(defun ef-uname (opt)
  "Exec uname with OPT and return the first line of the output as
a string."
  (car (split-string
        (shell-command-to-string (format "uname %s" opt))
        "\n")))

(defun ef-cpu-model (&optional n)
  "Return the cpu model as a string. N is the CPU index (start to
0)."
  (when (eq n nil)
    (setq n 0))

  (let* ((cmd "cat /proc/cpuinfo |grep 'model name'|uniq")
         (cpus (split-string (shell-command-to-string cmd) "\n"))
         (cpu (nth n cpus)))
    (if (string-equal cpu "")
        cpu
      (cadr (split-string cpu ": ")))))

(defun ef-gpu-model (&optional n)
  "Return the gpu model as a string. N is the GPU index (start to
0)."
  (when (equal n nil)
    (setq n 0))

  (if (not (eq (shell-command "lspci") 0))
      ""
    (let* ((cmd "lspci -mm |grep 'VGA'")
	   (lines (split-string (shell-command-to-string cmd) "\n"))
	   (line (nth n lines)))
      (if (string-equal line "")
	  line
	(let* ((gpu (split-string line "\""))
	       (corp (car (split-string (nth 3 gpu) " ")))
	       (model (nth 5 gpu)))
	  (format "%s %s" corp model))))))

(defun ef-computer ()
  "Return the computer model as a string."
  (let* ((file-family "/sys/devices/virtual/dmi/id/product_family")
         (file-name   "/sys/devices/virtual/dmi/id/product_name")
         (family (if (file-readable-p file-family)
                     (ef-get-first-line
                      (shell-command-to-string
                       (concat "cat " file-family)))
                   ""))
         (name (if (file-readable-p file-name)
                   (ef-get-first-line
                    (shell-command-to-string
                     (concat "cat " file-name)))
                 "")))
    (format "%s %s" name family)))

(defun ef-uptime ()
  "Return system uptime."
  (let* ((time (string-to-number
                (car (split-string (shell-command-to-string
                                    "cat /proc/uptime")))))
         (time-format (cond
                       ((< time 86400)
                        ;;Less than 1 day
                        "%h hours, %m minutes, %s seconds")
                       ((< time 31536000)
                        ;;Less than 1 year
                        (concat "%d days, %h hours, %m minutes, "
                                "%s seconds"))
                       (t
                        (concat "%y years, %d days, %h hours, "
                                "%m minutes, %s seconds")))))
    (format-seconds time-format time)))

(defun ef-load-avg ()
  "Return the average system load as string format."
  (if (file-exists-p "/proc/loadavg")
      (with-temp-buffer
	(insert-file-contents "/proc/loadavg")
	(let* ((line (buffer-substring-no-properties (line-beginning-position)
						     (line-end-position)))
	       (sline (split-string line " ")))
	  (concat (nth 0 sline) " "
		  (nth 1 sline) " "
		  (nth 2 sline))))
    ""))

(defun ef-shell ()
  "Return shell name as a string.

This function make the first letter of the shell name uppercase:
  \"bash\" -> \"Bash\"."
  (capitalize (file-name-nondirectory (getenv "SHELL"))))

(defun ef-desktop ()
  "Return desktop name."
  (let ((de (getenv "XDG_CURRENT_DESKTOP")))
    (cond
     ((string-match "GNOME" de)
      (let ((version (split-string (ef-get-first-line
                                    (shell-command-to-string
                                     "gnome-session --version")))))
        (concat de " " (car (last version)))))
     (t de))))

(defun ef-resolution-select-line (line)
  "Return useful LINE if is useful or nil."
  (let ((find (string-match "\\([0-9]*\\)[ ]+\\([0-9]*.[0-9]*[*]+\\)"
                            line)))
    (if (eq find nil)
        nil
      (remove "" (split-string line " ")))))

(defun ef-resolution-format-string (line)
  (format "%s " (nth 0 line)))

(defun ef-resolution ()
  (if (eq (shell-command "type -p xrandr") 0)
      (let ((data (split-string
                   (shell-command-to-string
                    "xrandr --nograb --current")
                   "\n")))
        (apply 'concat
               (mapcar 'ef-resolution-format-string
                       (remove nil
                               (mapcar 'ef-resolution-select-line
                                       data)))))
    ""))

(defun ef-distro ()
  "Return operating system name and version as string."
  (cond
   ;; Systemd distributions
   ((eq (file-readable-p "/etc/os-release") t)
    (cadr (split-string
           (shell-command-to-string
            "cat /etc/os-release |grep PRETTY_NAME")
           "\"")))
   ;; lsb_release
   ((eq (shell-command "type -p lsb_release") 0)
    (ef-get-first-line (shell-command-to-string "lsb_release -sd")))
   ;; Guix
   ((eq (shell-command "type -p guix") 0)
    (concat "GuixSD "
            (nth 4 (split-string
                    (shell-command-to-string
                     "guix system -V")
                    " \\|\n"))))
   ;; True OS
   ((file-exists-p "/etc/crontab.trueos")
    "TrueOS")
   ;; OS environment variable
   ((not (eq (getenv "OS") nil))
    (getenv "OS"))
   ;; OSTYPE environment variable
   ((not (eq (getenv "OSTYPE") nil))
    (getenv "OSTYPE"))
   ;; Default
   (t "Unknown OS")))

(defun ef-center-image (file)
  (let* ((img   (create-image file))
         (size  (image-size img))
         (width (car size))
         (left-margin (floor (- (window-total-width) width) 2)))
    (indent-to left-margin)
    (insert-image img)))

(defun ef-insert-os-image (os)
  "If `ef-custom-image' variable is set with image path, insert
this image, else insert image corresponding to OS distribution
with check of file existence. If no image is found, use
`default.png' file."
  (if (equal ef-custom-image "")
      (let* ((distro     (car (split-string os " ")))
             (distro-img (concat ef-images-dir distro ".png")))
        (if (file-exists-p distro-img)
	    (ef-center-image distro-img)
          (ef-center-image (concat ef-images-dir "default.png"))))
    (ef-center-image ef-custom-image)))

(defun ef-insert-os-ascii (os)
  "If `ef-custom-ascii' variable is set with ascii path, insert
this ascii-art, else insert image corresponding to OS
distribution with check of file existence. If no ascii is found,
use `default' file."
  (if (equal ef-custom-ascii "")
      (let* ((distro       (car (split-string os " ")))
             (distro-ascii (concat ef-ascii-dir distro)))
        (if (file-exists-p distro-ascii)
            (insert-file-contents distro-ascii)
          (insert-file-contents (concat ef-ascii-dir "default"))))
    (insert-file-contents ef-custom-ascii))
  ;; Move cursor
  (goto-char (point-max)))

(defun ef-installed-package (os)
  "Return package number as a string. This function detect
package manager with OS name."
  (cond
   ;; RPM
   ((string-match "Fedora\\|Red Hat" os)
    (concat (ef-get-first-line
             (shell-command-to-string
              "rpm -qa|wc -l"))
            " (RPM)"))
   ;; Dpkg
   ((string-match "Debian\\|Ubuntu\\|Trisquel" os)
    (concat (ef-get-first-line
             (shell-command-to-string
              "dpkg -l|wc -l"))
            " (Dpkg)"))
   ;; PacMan
   ((string-match "Manjaro\\|Arch" os)
    (concat (ef-get-first-line
	     (shell-command-to-string
	      "pacman -Qe|wc -l"))
	    " (PacMan)"))
   ;; Guix
   ((string-match "GuixSD" os)
    (let ((installed (ef-get-first-line
                      (shell-command-to-string
                       "guix package --list-installed|wc -l")))
          (available (if (eq ef-fast-display nil)
                         (concat
                          " / "
                          (ef-get-first-line
                           (shell-command-to-string
                            "guix package --list-available|wc -l")))
                       "")))
      (concat installed available " (Guix)")))
   (t "")))

(defun ef-login-host (&optional addline)
  "Return <login>@<host>. ADDLINE add new line separator like
  \"-------\n\"."
  (let ((login (format "%s@%s\n" (user-login-name) (system-name))))
    (if (equal addline t)
        (setq addline (concat (ef-add-spaces "" (- (length login) 1) "-")
                              "\n"))
      (setq addline ""))
    (concat login addline)))

(defun efetch-write-data ()
  "Insert efetch header and system information in current
buffer."
  (let* ((os       (ef-distro))
         (packages (ef-installed-package os))
         (data  `(("OS"     . ,os)
                  ("Host"   . ,(ef-computer))
                  ("Uptime" . ,(ef-uptime))
                  ("Kernel" . ,(ef-uname "-s"))
                  ("Kernel version" . ,(ef-uname "-r"))
                  ,(if (not (eq packages ""))
                       `("Packages" . ,packages))
                  ,(if (not (equal (getenv "DISPLAY") nil))
                       `("Desktop" . ,(ef-desktop)))
                  ,(if (not (equal (getenv "DISPLAY") nil))
                       `("Resolution" . ,(ef-resolution)))
                  ("Shell"  . ,(ef-shell))
                  ("Arch"   . ,(ef-uname "-m"))
                  ("CPU"    . ,(ef-cpu-model))
                  ,(if (not (equal (ef-cpu-model 1) ""))
                       `("CPU" . ,(ef-cpu-model 1)))
                  ("GPU"    . ,(ef-gpu-model))
                  ,(if (not (equal (ef-gpu-model 1) ""))
                       `("GPU" . ,(ef-gpu-model 1)))
                  ("Load average" . ,(ef-load-avg))
                  ("Emacs"  . ,(ef-emacs-info))
                  ("Emacs uptime" . ,(emacs-uptime)))))
    ;; Insert Header
    (if (eq graphic-session t)
    	(ef-insert-os-image os)
      (ef-insert-os-ascii os))
    (insert "\n" (ef-login-host t))

    ;; Insert data
    (mapc 'ef-display data)
    (mapc 'ef-display ef-additional-data)))

;;;###autoload
(defun efetch ()
  "Create new `efetch' buffer and write data."
  (interactive)
  (let* ((buff  (generate-new-buffer "efetch")))
    ;; Switch to new buffer and active efetch
    (set-buffer buff)
    (switch-to-buffer buff)
    (efetch-mode)

    ;; Insert data
    (efetch-write-data)

    ;; Set buffer to read only mode
    (read-only-mode t)))

(defun efetch-update ()
  "Update current `efetch' buffer."
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
    ("Efetch"     . font-lock-constant-face)
    ("\\(.*\\)@\\(.*\\)"  . font-lock-type-face))
  "Eftech-mode highlights.")

;;; Keymap
(defvar efetch-mode-map nil "Keymap for `efetch-mode'")
(setq efetch-mode-map (make-sparse-keymap))
(define-key efetch-mode-map (kbd "C-c C-c") 'efetch-update)

;;; Define mode
(define-derived-mode efetch-mode fundamental-mode "efetch"
  "Major mode for displaying efetch buffer."
  (setq font-lock-defaults '(efetch-highlights))
  (use-local-map efetch-mode-map))

(provide 'efetch)
;;; efetch.el ends here
