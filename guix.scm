;; Efetch -- Emacs major mode to display system information.
;;
;; Copyright (C) 2018 by Pierre-Antoine Rouby <contact@parouby.fr>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(use-modules ((guix licenses) #:prefix license:)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix gexp)
             (guix utils)
             (guix build-system emacs)
             (gnu packages)
             (gnu packages emacs))

(package
  (name "emacs-efetch")
  (version "0.0.3")
  (source (local-file "." "efetch"
                      #:recursive? #t))
  (build-system emacs-build-system)
  (arguments
   `(#:include '("\\.el$" "^images/")
     #:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'sed-el-in
         (lambda* (#:key outputs #:allow-other-keys)
           (substitute* "efetch-mode.el"
             (("~/.emacs.d/efetch-mode/images/")
              (string-append (assoc-ref outputs "out")
                             "/share/emacs/site-lisp/guix.d/"
                             "efetch-" ,version "/images/")))
           #t)))))
  (native-inputs `(("emacs" ,emacs-minimal)))
  (home-page "https://framagit.org/prouby/emacs-fetch")
  (synopsis "Major mode to display system information")
  (description "This Emacs packages able to display system
information.")
  (license license:gpl3+))
