;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;;
;;; This file is NOT part of GNU Guix, but is supposed to be used with GNU
;;; Guix and thus has the same license.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (non-free juicer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages java))

;; This depends on jCUDA, which depends on the non-free CUDA
;; libraries.
;;
;; TODO: unbundle all those jars in the lib directory.
(define-public java-juicebox
  (package
    (name "java-juicebox")
    (version "1.9.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aidenlab/Juicebox.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00nz7g9raj1nwdd6vcpq85kc88p7d4266knglf80sc8kjbwpv120"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "build.all.artifacts"
       #:tests? #f ; there are no tests
       #:make-flags
       (list (string-append "-Djdk.home.1.8=" (assoc-ref %build-inputs "jdk")))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "out/artifacts/")))))
    (home-page "https://github.com/aidenlab/Juicebox")
    (synopsis "Visualization and analysis software for Hi-C data")
    (description "Juicebox is visualization software for Hi-C data.
Juicebox can now be used to visualize and interactively (re)assemble
genomes.")
    (license license:expat)))
