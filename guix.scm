;;; guile-websocket --- WebSocket client/server
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of guile-websocket.
;;;
;;; Guile-websocket is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-websocket is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with guile-websocket.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To build it, but not install it, run:
;;
;;   guix build -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;;; Code:

(use-modules (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile))

(package
  (name "guile-websocket")
  (version "0.1")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://dthompson.us/guile-websocket.git")
                  (commit "d58d696")))
            (sha256
             (base32
              "10r8384frlyrljjdyzilrh8hzi60v9hisx4fxjs7rmg9g01cs77k"))))
  (build-system gnu-build-system)
  (arguments
   '(#:make-flags '("GUILE_AUTO_COMPILE=0")
     #:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'bootstrap
         (lambda _ (zero? (system* "sh" "bootstrap")))))))
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)))
  (inputs
   `(("guile" ,guile-2.0)))
  (synopsis "Websocket server/client for Guile")
  (description "Guile-websocket provides an implementation of the
WebSocket protocol as defined by RFC 6455.")
  (home-page "https://git.dthompson.us/guile-websocket.git")
  (license lgpl3+))
