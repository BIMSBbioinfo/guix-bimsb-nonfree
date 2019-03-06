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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (bimsb packages staging)
  #:use-module (bimsb packages variants)
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

;; Although this program is released under the GPL it depends on
;; ViennaRNA, which is non-free software.
(define-public mirdeep2
  (package
    (name "mirdeep2")
    (version "0.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rajewsky-lab/mirdeep2.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1driz4bd1v53p7wmmn3c8v7dg52xd8gw7sd5g6c1kclr4baxmmwc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; patch scripts checking for ../install_successful file
               (for-each
                (lambda (script)
                  (substitute* script
                    (("\\$bn/\\.\\./install_successful")
                     (string-append out
                                    "/share/mirdeep2/install_successful"))))
                '("src/mapper.pl"
                  "src/miRDeep2.pl"
                  "src/quantifier.pl"))
               ;; patch script using ../Rfam_for_miRDeep.fa file
               (substitute* "src/miRDeep2.pl"
                 (("\\$\\{scripts\\}/\\.\\./Rfam_for_miRDeep\\.fa")
                  (string-append out "/share/mirdeep2/Rfam_for_miRDeep.fa"))
                 (("\\$scripts/\\.\\./Rfam_for_miRDeep\\.fa")
                  (string-append out "/share/mirdeep2/Rfam_for_miRDeep.fa"))))
             ;; patch script using $(dirname `which miRDeep2.pl`)/indexes dir
             (substitute* "src/make_html.pl"
               (("`which miRDeep2\\.pl`")
                "`realpath ~/.local/share/mirdeep2` . \"/\"")
               (("mkdir \"\\$\\{scripts\\}indexes")
                "mkdir -p \"${scripts}indexes"))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "src"
                 (copy-recursively "." (string-append out "/bin")))
               ;; place Rfam_for_miRDeep.fa in /share/mirdeep2
               (mkdir-p (string-append out "/share/mirdeep2"))
               (copy-file
                "Rfam_for_miRDeep.fa"
                (string-append out "/share/mirdeep2/Rfam_for_miRDeep.fa"))
               ;; create install_successful file in share/mirdeep2
               (with-output-to-file
                   (string-append out "/share/mirdeep2/install_successful")
                 (const #t))) ;; simply touch the file
             #t))
         (add-after 'install 'wrap-perl-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Make sure perl scripts find all perl inputs at runtime.
               (for-each (lambda (prog)
                           (wrap-program (string-append out "/bin/" prog)
                             `("PERL5LIB" ":" prefix
                               (,(getenv "PERL5LIB")))))
                         '("make_html2.pl"
                           "make_html.pl"
                           "miRDeep2.pl"))
               ;; Make sure perl scripts find all input binaries at runtime.
               (for-each (lambda (prog)
                           (wrap-program (string-append out "/bin/" prog)
                             `("PATH" ":" prefix
                               (,(getenv "PATH")))))
                         '("make_html2.pl"
                           "make_html.pl"
                           "mapper.pl"
                           "miRDeep2_core_algorithm.pl"
                           "miRDeep2.pl"
                           "prepare_signature.pl"
                           "quantifier.pl"))
               #t))))))
    (inputs
     `(("bowtie1" ,bowtie1)
       ("perl-pdf-api2" ,perl-pdf-api2)
       ("perl" ,perl)
       ("randfold" ,randfold)
       ("viennarna" ,viennarna)))
    (synopsis "Discovering known and novel miRNAs from small RNA sequencing data")
    (description "miRDeep2 discovers active known or novel miRNAs from deep
sequencing data (Solexa/Illumina, 454, ...).")
    (home-page "https://www.mdc-berlin.de/8551903/en/")
    (license license:gpl3+)))

;; Although this program is released under the GPL it depends on
;; ViennaRNA, which is non-free software.
(define-public locarna
  (package
    (name "locarna")
    (version "1.8.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.bioinf.uni-freiburg.de/"
                                  "Software/LocARNA/Releases/locarna-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0rq53xd8v1wqcbhj8g2lqir2z0nk16pcli6x4bj5xzlbsimy86ri"))))
    (build-system gnu-build-system)
    (inputs
     `(("file" ,file)
       ("perl" ,perl)
       ("viennarna" ,viennarna)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (synopsis "RNA alignment tools")
    (description
     "LocARNA is a collection of alignment tools for the structural
analysis of RNA.  Given a set of RNA sequences, LocARNA simultaneously
aligns and predicts common structures for your RNAs.  In this way,
LocARNA performs Sankoff-like alignment and is in particular suited
for analyzing sets of related RNAs without known common structure.")
    (home-page "http://www.bioinf.uni-freiburg.de/Software/LocARNA/")
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
     `(("ruby" ,ruby)
       ("boost" ,boost-1.58)
       ("viennarna" ,viennarna-1.8)))
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

;; Although this is free software, it depends on rbowtie, which is
;; nonfree.
(define-public r-quasr
  (package
    (name "r-quasr")
    (version "1.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "QuasR" version))
       (sha256
        (base32
         "15lnrj8m8p1ns7iv5f2j0xshma3gpjp3lwry1s0axsxsk9khzrl0"))))
    (properties `((upstream-name . "QuasR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocmanager" ,r-biocmanager)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rbowtie" ,r-rbowtie) ; non-free
       ("r-rsamtools" ,r-rsamtools)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-shortread" ,r-shortread)
       ("r-zlibbioc" ,r-zlibbioc)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://bioconductor.org/packages/QuasR")
    (synopsis "Quantify and annotate short reads in R")
    (description
     "This package provides a framework for the quantification and
analysis of short genomic reads.  It covers a complete workflow
starting from raw sequence reads, over creation of alignments and
quality control plots, to the quantification of genomic regions of
interest.")
    (license license:gpl2)))
