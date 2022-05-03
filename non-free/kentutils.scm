;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2021, 2022 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
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

(define-module (non-free kentutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix licenses-nonfree) #:prefix nonfree:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages tls))

(define-public kentutils-nonfree
  (package
    (name "kentutils-nonfree")
    (version "430")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://genome-source.soe.ucsc.edu/kent.git")
                    (commit (string-append "v" version "_base"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a9dz4xkb7gsdf644kfcpdj8wcqk2fl04hh7zkpw5z5ffgqwg50w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       ,#~(list "CC=gcc"
                "CFLAGS=-fcommon -DUCSC_CRAM=1 -DKNETFILE_HOOKS -mpopcnt"
                (string-append "DESTDIR=" #$output)
                (string-append "DOCUMENTROOT=" #$output "/share/kentutils/htdocs")
                (string-append "SCRIPTS=" #$output "/share/kentutils/scripts")
                (string-append "CGI_BIN=" #$output "/share/kentutils/cgi-bin"))
       #:tests? #f ; none included
       #:phases
       (modify-phases %standard-phases
         ;; The sources are expected to be found inside the "kent"
         ;; subdirectory.
         (replace 'configure
           (lambda _
             (rename-file "src/userApps" "buildroot/")
             (mkdir "buildroot/kent")
             (rename-file "src" "buildroot/kent/src")
             (for-each make-file-writable
                       (find-files "buildroot" ".*"))
             (chdir "buildroot")))
         (add-before 'build 'build-libs
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "-C" "kent/src" "libs" make-flags)))
         ;; By setting DESTDIR the binaries are built directly in the
         ;; target directory.  There is no separate installation step.
         (delete 'install))))
    (inputs
     (list openssl
           libpng
           (list util-linux "lib")
           mysql))
    (native-inputs
     (list rsync))
    (home-page "https://www.soe.ucsc.edu")
    (synopsis "UCSC genome browser bioinformatic utilities")
    (description "This package provides the command line
bioinformatics utilities from the kent source tree used in the UCSC
genome browser.")
    ;; no commercial usage permitted
    (license (nonfree:non-free "file://licenseUcscGenomeBrowser.txt"))))
