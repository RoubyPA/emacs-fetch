;;; efetch-mode.el --- Efetch major mode.            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Pierre-Antoine Rouby
;; Copyright (C) 2018  David Tabarie

;; Author: Pierre-Antoine Rouby <contact@parouby.fr>
;; Author: David Tabarie <david.tabarie@gmail.com>
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

(defconst efetch-version "0.0.3"
  "Current efetch version.")

(defvar ef-images-dir
  "~/.emacs.d/efetch-mode/images/"
  "Efetch images directory.")

(defvar ef-margin 15
  "Eftech margin between start of line and data.")

(defvar ef-separator " : "
  "Efetch separator in string.")

(defvar ef-distro-image '(("GuixSD" . "guix.png")
                          ("Debian" . "debian.png")
                          ("Ubuntu" . "ubuntu.png")
                          ("Trisquel" . "trisquel.png")
                          (t . "default.png"))
  "Used to associate each OS to the corresponding image.

Each elements is an associated list, like :
  (\"OS name\" . \"image.png\")

The t value is used when no corresponding image is found, or when
the OS is unknown.")

(defvar ef-custom-image ""
  "Path of custom image to replace distro image. By default this
variable is empty.")

(defvar ef-additional-data '()
  "Additional data list for efetch buffer.

Data list is a list of associated list, like:
  '((\"name\"  . \"value\")
    (\"name2\" . \"value2)")

(defvar ef-fast-display nil
  "When is t disable time expensive function.")

(defun ef-get-first-line (str)
  "Return first line of STR."
  (car (split-string str "\n")))

(defun ef-file-readable? (filename)
  "Return t if FILENAME exists and user have right to read it,
else return nil."
  (if (file-exists-p filename)
      (if (eq 0 (string-match "-r..r..r.."
                             (file-attribute-modes
                              (file-attributes filename))))
          t
        nil)
    nil))

(defun ef-add-spaces (str n &optional sep)
  "Add N spaces to STR."
  (when (equal sep nil)
    (setq sep " "))
  (if (= n 0)
      str
    (ef-add-spaces (concat str sep)
                   (- n 1) sep)))

(defun ef-display (l)
  "Display new line in eftech buffer.

L is a list like (\"key\" . \"value\")."
  (unless (or (equal l '())
              (equal l nil))
    (insert
     (concat (ef-add-spaces (car l) (- ef-margin (length (car l))))
             ef-separator (cdr l) "\n"))))

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

  (let* ((cmd "lspci -mm |grep 'VGA'")
         (lines (split-string (shell-command-to-string cmd) "\n"))
         (line (nth n lines)))
    (if (string-equal line "")
        line
      (let* ((gpu (split-string line "\""))
             (corp (car (split-string (nth 3 gpu) " ")))
             (model (nth 5 gpu)))
        (format "%s %s" corp model)))))

(defun ef-computer ()
  "Return the computer model as a string."
  (let* ((file-family "/sys/devices/virtual/dmi/id/product_family")
         (file-name   "/sys/devices/virtual/dmi/id/product_name")
         (family (if (ef-file-readable? file-family)
                     (ef-get-first-line
                      (shell-command-to-string
                       (concat "cat " file-family)))
                   ""))
         (name (if (ef-file-readable? file-name)
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
  (with-temp-buffer
    (insert-file-contents "/proc/loadavg")
    (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))
           (sline (split-string line " ")))
      (concat (nth 0 sline) " "
              (nth 1 sline) " "
              (nth 2 sline)))))

(defun ef-shell ()
  "Return shell name as a string.

This function make the first letter of the shell name uppercase:
  \"bash\" -> \"Bash\"."
  (capitalize (file-name-nondirectory (getenv "SHELL"))))

(defun ef-desktop ()
  "Return desktop name."
  (capitalize (downcase (getenv "XDG_CURRENT_DESKTOP"))))

(defun ef-resolution-select-line (line)
  (let ((find (string-match "\\([0-9]*\\)[ ]+\\([0-9]*.[0-9]*[*]+\\)"
                            line)))
    (if (eq find nil)
        nil
      line)))

(defun ef-resolution-list-of-data (line)
  (remove "" (split-string line " ")))

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
                       (mapcar 'ef-resolution-list-of-data
                               (remove nil
                                       (mapcar 'ef-resolution-select-line
                                               data))))))
    ""))

(defun ef-distro ()
  "Return operating system name and version as string."
  (cond
   ;; Systemd distributions
   ((eq (ef-file-readable? "/etc/os-release") t)
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
   ;; Default
   (t "Unknown OS")))

(defun ef-insert-os-image (os)
  "Insert image corresponding to OS distribution with the
`ef-distro-image' variable. If no image is found, use default
one."
  (if (equal ef-custom-image "")
      (let* ((distro (car (split-string os " ")))
             (search (assoc distro ef-distro-image))
             (image  (if search
                         (cdr search)
                       (cdr (assoc t ef-distro-image)))))
        (insert-image
         (create-image (concat ef-images-dir image))))
    (insert-image
     (create-image ef-custom-image))))

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
  "Insert efetch header and system information in current buffer."
  (let* ((os       (ef-distro))
         (packages (ef-installed-package os))
         (data  `(("OS"     . ,os)
                  ("Host"   . ,(ef-computer))
                  ,(if (not (eq packages ""))
                       `("Packages" . ,packages))
                  ("Uptime" . ,(ef-uptime))
                  ("Kernel" . ,(ef-uname "-s"))
                  ("Kernel version" . ,(ef-uname "-r"))
                  ("Arch"   . ,(ef-uname "-m"))
                  ("CPU"    . ,(ef-cpu-model))
                  ,(if (not (equal (ef-cpu-model 1) ""))
                       `("CPU" . ,(ef-cpu-model 1)))
                  ("GPU"    . ,(ef-gpu-model))
                  ,(if (not (equal (ef-gpu-model 1) ""))
                       `("GPU" . ,(ef-gpu-model 1)))
                  ("Load average" . ,(ef-load-avg))
                  ("Shell"  . ,(ef-shell))
                  ("Resolution" . ,(ef-resolution))
                  ,(if (not (equal (getenv "DISPLAY") nil))
                       `("Desktop" . ,(ef-desktop)))
                  ("Emacs"  . ,(ef-emacs-info))
                  ("Emacs uptime" . ,(emacs-uptime)))))
    ;; Insert Header
    (insert "+++ Efetch +++\n")
    (ef-insert-os-image os)
    (insert "\n")                       ;New line after image
    (insert (ef-login-host t))

    ;; Insert data
    (mapc 'ef-display data)
    (mapc 'ef-display ef-additional-data)))

;;;###autoload
(defun efetch ()
  "Create new `efetch' buffer and write data."
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
;;; efetch-mode.el ends here
