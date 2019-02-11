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
             (gnu packages base)
             (gnu packages emacs)
             (gnu packages xorg))

(package
  (name "emacs-efetch")
  (version "0.0.5")
  (source (local-file "." "efetch"
                      #:recursive? #t))
  (build-system emacs-build-system)
  (arguments
   `(#:include '("\\.el$" "^images/" "^ascii-arts/")
     ;; TODO Run tests
     ;; #:tests? #t
     ;; #:emacs ,emacs
     ;; #:test-command '("make" "check")
     ))
  (native-inputs `(("emacs" ,emacs-minimal)
                   ;; Need for tests
                   ("make" ,gnu-make)))
  (propagated-inputs `(("xrandr" ,xrandr)))
  (home-page "https://framagit.org/prouby/emacs-fetch")
  (synopsis "Emacs interface to display system information")
  (description "This Emacs packages provide an interface for
visualising system information in emacs")
  (license license:gpl3+))
