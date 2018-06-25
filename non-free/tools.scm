;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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

(define-module (non-free tools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix licenses-nonfree) #:prefix nonfree:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages))

(define-public iozone
  (package
    (name "iozone")
    (version "3.482")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.iozone.org/src/current/"
                                  "iozone"
                                  (string-map (lambda (c)
                                                (if (char=? c #\.) #\_ c))
                                              version)
                                  ".tar"))
              (sha256
               (base32
                "0c5fyfr3iqfb8my7dcjlhm6kkmcl4a7r6gcgqrvp3xwn7jvgwcr7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "linux" "CC=gcc")
       #:tests? #f ; none included
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "src/current/")))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (for-each (lambda (file)
                           (install-file file bin))
                         '("iozone" "fileop" "pit_server"))
               (install-file "../../docs/iozone.1" man1))
             #t)))))
    (home-page "http://www.iozone.org")
    (synopsis "Filesystem benchmark tool")
    (description "IOzone is a filesystem benchmark tool.  The benchmark
generates and measures a variety of file operations.  Iozone is useful for
performing a broad filesystem analysis of a vendor’s computer platform.  The
benchmark tests file I/O performance for the following operations: Read,
write, re-read, re-write, read backwards, read strided, @code{fread},
@code{fwrite}, random read, @code{pread}, @code{mmap}, @code{aio_read},
@code{aio_write}.")
    (license (nonfree:non-free
              "http://www.iozone.org/docs/Iozone_License.txt"))))
