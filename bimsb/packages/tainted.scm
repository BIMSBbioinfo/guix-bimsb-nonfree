;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2022 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2019, 2020, 2021 Marcel Schilling <marcel.schilling@uni-luebeck.de>
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
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (past packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (bimsb packages staging)
  #:use-module (bimsb packages variants)
  #:use-module (bimsb packages bioinformatics-nonfree)
  #:use-module (non-free cuda)
  #:use-module (guix-science-nonfree packages bioconductor)
  #:use-module (guix-science-nonfree packages bioinformatics))

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
     (list
      #:tests? #false                      ; no "check" target
      #:phases
      #~(modify-phases %standard-phases
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
                 (string-append "#!" (which "Rscript") "\n" m)))))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
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
                    `("R_LIBS_SITE" ":" prefix (,(getenv "R_LIBS_SITE")))))))))))
    (inputs
     (list coreutils
           perl
           perl-carp
           perl-czplib
           perl-math-cdf                ; non-free
           perl-data-dumper
           perl-getopt-long
           gzip
           r-minimal
           r-getopt))
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

;; Although this is free software, it depends on rankprod, which is
;; nonfree.
(define-public r-translatome
  (package
    (name "r-translatome")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "tRanslatome" version))
       (sha256
        (base32
         "169nizkxgs9dyhyria6354jvxr9hmqq2y00kgril4sa0kkhwma4y"))))
    (properties `((upstream-name . "tRanslatome")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-anota
           r-biobase
           r-deseq
           r-edger
           r-gosemsim
           r-gplots
           r-heatplus
           r-limma
           r-org-hs-eg-db
           r-plotrix
           r-rankprod                   ; non-free
           r-sigpathway
           r-topgo))
    (home-page "https://bioconductor.org/packages/tRanslatome/")
    (synopsis "Comparison between multiple levels of gene expression")
    (description
     "This package is used for the detection of @dfn{differentially
expressed genes} (DEGs) from the comparison of two biological
conditions (treated vs. untreated, diseased vs. normal, mutant
vs. wild-type) among different levels of gene
expression (transcriptome ,translatome, proteome), using several
statistical methods: Rank Product, Translational Efficiency, t-test,
Limma, ANOTA, DESeq, edgeR.  It also provides the possibility to plot
the results with scatterplots, histograms, MA plots, standard
deviation (SD) plots, coefficient of variation (CV) plots.")
    (license license:gpl3)))

;; Although this program is released under the GPL it depends on
;; ViennaRNA, which is non-free software.
(define-public mirdeep2
  (package
    (name "mirdeep2")
    (version "0.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rajewsky-lab/mirdeep2.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mlihj6c6zkqsvrdqnz5wjz4annjr8yi6slv63bbcl4bdwdywcd6"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no check target; instead: test after install (see below)
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              ;; patch scripts checking for ../install_successful file
              (substitute* '("src/mapper.pl" "src/miRDeep2.pl"
                             "src/quantifier.pl")
                (("(\\$a=)`which (miRDeep2\\.pl)`" _ lhs script)
                 (string-append lhs "\"" #$output "/bin/" script "\""))
                (("\\$bn/\\.\\./install_successful")
                 (string-append #$output "/share/mirdeep2/install_successful")))
              ;; patch script using ../Rfam_for_miRDeep.fa file
              (substitute* "src/miRDeep2.pl"
                (("\\$\\{?scripts\\}?/\\.\\.(/Rfam_for_miRDeep\\.fa)" _ file)
                 (string-append #$output "/share/mirdeep2" file))
                ;; Comment out all lines featuring the `scripts` variable for
                ;; paths relative to the script.
                ((".*\\$scripts.*" line)
                 (string-append "#" line)))
              ;; patch script using $(dirname `which miRDeep2.pl`)/indexes dir
              (substitute* "src/make_html.pl"
                (("`which miRDeep2\\.pl`")
                 "`realpath ~/.local/share/mirdeep2` . \"/\"")
                (("mkdir (\"\\$\\{scripts\\}indexes)" _ path)
                 (string-append "mkdir -p " path)))))
          (replace 'install
            (lambda _
              (with-directory-excursion "src"
                (copy-recursively "." (string-append #$output "/bin")))
              ;; place Rfam_for_miRDeep.fa in /share/mirdeep2
              (mkdir-p (string-append #$output "/share/mirdeep2"))
              (copy-file
               "Rfam_for_miRDeep.fa"
               (string-append #$output "/share/mirdeep2/Rfam_for_miRDeep.fa"))
              ;; create install_successful file in share/mirdeep2
              (with-output-to-file
                  (string-append #$output "/share/mirdeep2/install_successful")
                (const #t))))
          (add-after 'install 'wrap-perl-scripts
            (lambda _
              ;; Make sure perl scripts find all perl inputs at runtime.
              (for-each (lambda (prog)
                          (wrap-program (string-append #$output "/bin/" prog)
                            `("PERL5LIB" ":" prefix
                              (,(getenv "PERL5LIB")))))
                        '("make_html2.pl"
                          "make_html.pl"
                          "miRDeep2.pl"))
              ;; Make sure perl scripts find all input binaries at runtime.
              (for-each (lambda (prog)
                          (wrap-program (string-append #$output "/bin/" prog)
                            `("PATH" ":" prefix
                              (,(getenv "PATH")))))
                        '("make_html2.pl"
                          "make_html.pl"
                          "mapper.pl"
                          "miRDeep2_core_algorithm.pl"
                          "miRDeep2.pl"
                          "prepare_signature.pl"
                          "quantifier.pl"))))
          (add-after 'wrap-perl-scripts 'test
            (lambda _
              (setenv "PATH" (string-append #$output "/bin:"
                                            (getenv "PATH")))
              (with-directory-excursion "tutorial_dir"
                ;; Script writes to "~/.local/share/mirdeep2", but
                ;; HOME=/homeless-shelter by default:
                (setenv "HOME" ".")
                (mkdir-p ".local/share/mirdeep2")
                ;; Run script in bash to processs output for error detection:
                (invoke
                 "bash" "-c"
                 (string-append
                  ;; Script always returns 0, reports errors to stdout/stderr.
                  "./run_tut.sh 2>&1"
                  ;; Copy (combined) output to stderr for build log:
                  " | while read output; do"
                  " echo \"$output\";"
                  " echo \"$output\" >&2; "
                  " done"
                  ;; Check output (the copy on stdout) for error messages.
                  " | grep -q"
                  ;; Printed to stdout when the script detects an error:
                  " -e 'An error occured'"
                  ;; Ignored by the script, but on stderr:
                  " -e 'No such file or directory'"
                  " -e 'Permission denied'"
                  ;; Any error message found: failure; else: success.
                  "&& exit 1; exit 0"))))))))
    (inputs
     (list bowtie1
           perl-pdf-api2
           perl
           randfold
           viennarna))
    (home-page "https://www.mdc-berlin.de/8551903/en/")
    (synopsis "Discovering known and novel miRNAs from small RNA sequencing data")
    (description "miRDeep2 discovers active known or novel miRNAs from deep
sequencing data (Solexa/Illumina, 454, ...).")
    (license license:gpl3+)))

;; Although this program is released under the GPL it depends on
;; ViennaRNA, which is non-free software.
(define-public locarna
  (package
    (name "locarna")
    (version "1.9.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/s-will/LocARNA"
                                  "/releases/download/v" version
                                  "/locarna-" version ".tar.gz"))
              (sha256
               (base32
                "17k1xlki4jravm6c98vm82klzxhw33fg4ibyh4n4czlsaz0hvqh8"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false ;XXX: requires tcoffee
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-reference-to-/bin/cp
            (lambda _
              (substitute* "src/Utils/mlocarna"
                (("/bin/cp") (which "cp"))))))))
    (inputs
     (list file
           perl
           viennarna))
    (native-inputs
     (list autoconf
           automake
           libtool
           doxygen
           which
           pkg-config))
    (home-page "http://www.bioinf.uni-freiburg.de/Software/LocARNA/")
    (synopsis "RNA alignment tools")
    (description
     "LocARNA is a collection of alignment tools for the structural
analysis of RNA.  Given a set of RNA sequences, LocARNA simultaneously
aligns and predicts common structures for your RNAs.  In this way,
LocARNA performs Sankoff-like alignment and is in particular suited
for analyzing sets of related RNAs without known common structure.")
    (license license:gpl3)))

;; This software is released under the GPL but depends on the non-free
;; ViennaRNA, so we cannot add it to Guix upstream.
(define-public ensembleclust
  (package
    (name "ensembleclust")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://bpla-kernel.dna.bio.keio.ac.jp"
                                  "/clustering/EnsembleClust-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "11jfbqkyvk2agq7q9lvblqif299pwwc8lvn6d33jd9gy1hcrwzjr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "BOOST_LIBS=-lboost_system")
       #:make-flags
       ;; This program was written for an older version of Boost.
       '("CPPFLAGS=-DBOOST_SPIRIT_USE_OLD_NAMESPACE -fpermissive")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-boost-includes
           (lambda _
             (substitute* '("src/similarity/common/fa.h"
                            "src/similarity/common/fa.cpp"
                            "src/similarity/common/maf.h"
                            "src/similarity/common/maf.cpp"
                            "src/similarity/common/aln.h"
                            "src/similarity/common/aln.cpp"
                            "src/similarity/bpla_kernel/data.h")
               (("boost/spirit/")
                "boost/spirit/home/classic/"))
             (substitute* "src/similarity/bpla_kernel/main.cpp"
               ((" _1")
                " boost::lambda::_1"))
             #t))
         (add-after 'patch-boost-includes 'chdir
           (lambda _ (chdir "src/similarity") #t))
         (add-after 'install 'chdir-wpgma
           (lambda _ (chdir "../../src/wpgma") #t))
         (add-after 'chdir-wpgma 'configure-wpgma
           (lambda* (#:key configure-flags
                     #:allow-other-keys #:rest args)
             (let* ((configure (assoc-ref %standard-phases 'configure)))
               (apply configure
                      (append args
                              (list #:configure-flags
                                    configure-flags))))))
         (add-after 'configure-wpgma 'build-wpgma
           (lambda* (#:key make-flags
                     #:allow-other-keys #:rest args)
             (let* ((build (assoc-ref %standard-phases 'build)))
               (apply build
                      (append args
                              (list #:make-flags make-flags))))))
         (add-after 'build-wpgma 'install-wpgma
           (lambda* (#:key make-flags
                     #:allow-other-keys #:rest args)
             (let* ((install (assoc-ref %standard-phases 'install)))
               (apply install
                      (append args
                              (list #:make-flags make-flags))))))
         (add-after 'install-wpgma 'install-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/ensembleclust"))
                    (bin (string-append out "/bin")))
               (mkdir-p share)
               (copy-recursively "../script" share)
               (mkdir-p bin)

               ;; Generate wrapper script.  We cannot use the script
               ;; that comes with the sources because the paths are
               ;; all wrong.
               (call-with-output-file (string-append bin "/cluster")
                 (lambda (port)
                   (display (string-append
                             "#!" (which "sh")
                             bin "/bpla_kernel result/$3.mat +1 $1\n"
                             share "/mat2dist.rb result/$3.mat > result/$3.dist\n"
                             bin "/pgma $2 result/$3.dist > result/$3.tree\n")
                            port)))
               (chmod (string-append bin "/cluster") #o755)
               #t))))))
    (inputs
     (list ruby
           boost-1.58
           viennarna-1.8))
    (synopsis "Clustering of non-coding RNAs")
    (description
     "EnsembleClust provides tools for fast and accurate clustering of
non-coding RNAs.  This is achieved by means of a new similarity
measure for the hierarchical clustering of ncRNAs.  The idea is that
the reliability of approximate algorithms can be improved by utilizing
the information of suboptimal solutions in their dynamic programming
frameworks.  EnsembleClust utilizes all possible sequence alignments
and all possible secondary structures.")
    (home-page "http://bpla-kernel.dna.bio.keio.ac.jp/clustering/")
    (license license:gpl2+)))

;; This software is released under the GPL but depends on the non-free
;; ViennaRNA, so we cannot add it to Guix upstream.
(define-public rnanue
  (let ((commit "7fb7ab98d9d0f67fc61e5f0e728ad4b6da0833b1")
        (revision "1"))
    (package
      (name "rnanue")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Ibvt/RNAnue/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1kcab00xsgz1rd3shjsa8s23pqcpqzwqfagl8qvp6g9ddxz1d5ii"))
                (modules '((guix build utils)))
                (snippet
                 '(delete-file-recursively "build"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #false ; there are none
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-source-directory
             (lambda _
               (chdir "source")))
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "RNAnue" bin)))))))
      (inputs
       `(("boost" ,boost)
         ("segemehl" ,segemehl)
         ("seqan" ,seqan)
         ("viennarna" ,viennarna)))
      (home-page "https://github.com/Ibvt/RNAnue")
      (synopsis "Detect RNA-RNA interactions from Direct-Duplex-Detection data")
      (description
       "RNAnue is a comprehensive analysis to detect RNA-RNA
interactions from @dfn{Direct-Duplex-Detection} (DDD) data.")
      (license license:gpl3+))))

;; TODO: this is not reproducible.
(define-public bart-with-cuda
  (package
    (name "bart-with-cuda")
    (version "0.7.00")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mrirecon/bart")
             (commit "d1b0e576c3f759089915565d5bf57832acf7b03e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "159rj3agr9pb9lg38b56rnw3d8wcbkmb2n718z26zpy4c6a6d9rn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "utest"
      #:make-flags #~(list
                      (string-append "PREFIX=" #$output)
                      "CUDA=1"
                      (string-append "CUDA_BASE=" #$(this-package-input "cuda-toolkit"))
                      "OPENBLAS=1"
                      "SCALAPACK=1"
                      (string-append "BLAS_BASE=" #$(this-package-input "openblas"))
                      (string-append "FFTW_BASE=" #$(this-package-input "fftw")))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-/bin/bash
            (lambda _
              (substitute* "tests/pics.mk"
                (("/bin/bash") (which "bash"))))))))
    (inputs
     (list cuda
           fftw
           fftwf
           libpng
           openblas
           python
           scalapack))
    (native-inputs
     (list gcc-8
           doxygen
           util-linux))                 ;for flock
    (home-page "https://mrirecon.github.io/bart/")
    (synopsis "Toolbox for computational magnetic resonance imaging")
    (description "The Berkeley Advanced Reconstruction Toolbox (BART) is an
image-reconstruction framework for Computational Magnetic Resonance Imaging.
The tools in this software implement various reconstruction algorithms for
Magnetic Resonance Imaging.")
    (license license:bsd-3)))
