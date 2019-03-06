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

(define-module (bimsb packages tainted)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages statistics)
  #:use-module (bimsb packages bioinformatics-nonfree))

;; Tainted because of the dependency on the non-free Math::CDF.
(define-public mrin
  (package
    (name "mrin")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chaolinzhanglab/mrin.git")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "183k8a753fjd6fdv1jf3n5rxfry11358kl98k76z6dvm3ggrz651"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (substitute* '("gen_transcript_cdf.pl"
                            "ks_test_uniform.pl")
               (("gunzip -c")
                (string-append (which "gunzip") " -c")))
             (substitute* "tag2profile.pl"
               (("mkdir") (which "mkdir"))
               (("rm -rf") (string-append (which "rm") " -rf")))
             (substitute* "cal_mrin.R"
               (("^suppressMessages" m)
                (string-append "#!" (which "Rscript") "\n" m)))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (for-each (lambda (file)
                           (install-file file bin)
                           (chmod file #o555)
                           (wrap-program (string-append bin "/" (basename file))
                             `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB")))))
                         '("gen_ks_matrix.pl" "gen_transcript_cdf.pl"
                           "ks_test_uniform.pl" "tag2profile.pl"))
               (let ((file "cal_mrin.R"))
                 (install-file file bin)
                 (chmod file #o555)
                 (wrap-program (string-append bin "/" (basename file))
                   `("R_LIBS_SITE" ":" prefix (,(getenv "R_LIBS_SITE"))))))
             #t)))))
    (inputs
     `(("coreutils" ,coreutils)
       ("perl" ,perl)
       ("perl-carp" ,perl-carp)
       ("perl-czplib" ,perl-czplib)
       ("perl-math-cdf" ,perl-math-cdf) ; non-free
       ("perl-data-dumper" ,perl-data-dumper)
       ("perl-getopt-long" ,perl-getopt-long)
       ("gzip" ,gzip)
       ("r" ,r-minimal)
       ("r-getopt" ,r-getopt)))
    (home-page "http://zhanglab.c2b2.columbia.edu/index.php/MRIN")
    (synopsis "Tool to obtain quantitative measure of mRNA integrity")
    (description
     "MRIN is a computational method to assess a quantitative measure of mRNA
integrity, named mRIN (mRNA integrity number).  This is done by quantitatively
modeling of the 3' bias of read coverage profiles along each mRNA transcript.
A per-sample summary mRIN is then derived as an indicator of mRNA degradation.
This method has been used for systematic analysis of large scale RNA-Seq data
of postmortem tissues, in which RNA degradation during tissue collection is
particularly an issue.")
    (license license:gpl3)))
