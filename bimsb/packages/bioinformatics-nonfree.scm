;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2017 CM Massimo <carlomaria.massimo@mdc-berlin.de>
;;; Copyright © 2018, 2019, 2021 Marcel Schilling <marcel.schilling@uni-luebeck.de>
;;; Copyright © 2023 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
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

(define-module (bimsb packages bioinformatics-nonfree)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix licenses-nonfree) #:prefix nonfree:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system r)
  #:use-module (bimsb packages staging)
  #:use-module (bimsb packages tainted)
  #:use-module (bimsb packages variants)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages image)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix-science-nonfree packages bioinformatics)
  #:use-module (past packages boost)
  #:use-module (past packages maths)
  #:use-module (past packages perl)
  #:use-module (past packages python27))

(define (other-perl-package-name other-perl)
  "Return a procedure that returns NAME with a new prefix for
OTHER-PERL instead of \"perl-\", when applicable."
  (let ((other-perl-version (version-major+minor
                             (package-version other-perl))))
    (lambda (name)
      (if (string-prefix? "perl-" name)
          (string-append "perl" other-perl-version "-"
                         (string-drop name
                                      (string-length "perl-")))
          name))))

(define-public (package-for-other-perl other-perl pkg)
  ;; This is a procedure to replace PERL by OTHER-PERL, recursively.
  ;; It also ensures that rewritten packages are built with OTHER-PERL.
  (let* ((rewriter (package-input-rewriting `((,perl . ,other-perl))
                                            (other-perl-package-name other-perl)
                                            #:deep? #false))
         (new (rewriter pkg)))
    (package (inherit new)
      (arguments `(#:perl ,other-perl
                   #:tests? #f ; oh well...
                   ,@(package-arguments new))))))

(define-public bcl2fastq
  (package
    (name "bcl2fastq")
    (version "2.18.0.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://support.illumina.com/content/"
                                  "dam/illumina-support/documents/downloads/"
                                  "software/bcl2fastq/bcl2fastq2-v"
                                  (string-join (string-split version #\.) "-")
                                  "-tar.zip"))
              (sha256
               (base32
                "0anshb1qvpzm373q338qgr0gs1bjpw4ssyysl4gh7nfwidzmca25"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DBCL2FASTQ_VERSION:STRING=" ,version)
             "-DBCL2FASTQ_NAME_SHORT:STRING=bcl2fastq"
             "-DBCL2FASTQ_NAME_LONG:STRING=BCL to FASTQ file converter"
             "-DBCL2FASTQ_COPYRIGHT:STRING=Copyright (c) 2007-2016 Illumina, Inc."
             (string-append "-DBCL2FASTQ_SOURCE_DIR:STRING=" (getcwd) "/bcl2fastq/src"))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "unzip" source)
             (invoke "tar" "-xvf"
                     (string-append "bcl2fastq2-v"
                                    ,version ".tar.gz"))))
         (add-after 'unpack 'enter-dir (lambda _ (chdir "bcl2fastq/src") #t))
         (add-after 'enter-dir 'patch-stuff
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Update for boost 1.54 -> 1.56
             (substitute* "cxx/lib/io/Xml.cpp"
               (("xml_writer_make_settings\\(")
                "xml_writer_make_settings<ptree::key_type>("))
             (substitute* "cxx/include/common/Logger.hh"
               (("#include <ios>" m)
                (string-append "#include <iostream>\n" m)))
             ;; Do not use bundled libraries
             (substitute* "cmake/cxxConfigure.cmake"
               (("\"\\$\\{LIBEXSLT_LIBRARIES\\}\"")
                (string-append (assoc-ref inputs "libxslt")
                               "/lib/libexslt.so"))
               (("find_library_redist\\(LIBXSLT .*")
                "bcl2fastq_find_library(LIBXSLT libxslt/xsltconfig.h xslt)\n")
               (("find_library_redist\\(LIBXML2 .*")
                "bcl2fastq_find_library(LIBXML2 libxml/xpath.h xml2)\n")
               (("find_library_redist\\(LIBEXSLT .*")
                "bcl2fastq_find_library(LIBEXSLT libexslt/exslt.h exslt)\n")
               (("redist_package") "#")
               (("^  +\"--prefix=.*") ""))
             ;; Work around broken version checking
             (substitute* "CMakeLists.txt"
               (("BCL2FASTQ_LIBXML2_VERSION 2.7.8")
                ,(string-append "BCL2FASTQ_LIBXML2_VERSION "
                                (package-version libxml2)))
               (("BCL2FASTQ_LIBXSLT_VERSION 1.1.26")
                ,(string-append "BCL2FASTQ_LIBXSLT_VERSION "
                                (package-version libxslt)))))))))
    (inputs
     `(("boost" ,boost-1.58)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("zlib" ,zlib)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://support.illumina.com/downloads/bcl2fastq_conversion_software.html")
    (synopsis "Convert files in BCL format to FASTQ")
    (description
     "bcl2fastq is conversion software, which can be used to both
demultiplex data and convert BCL files to FASTQ files.")
    (license (nonfree:non-free
              (string-append "http://support.illumina.com/content/dam"
                             "/illumina-support/documents/documentation"
                             "/software_documentation/bcl2fastq/"
                             "bcl2fastq2-v2-16-EULA.pdf")
              "This is an extremely restrictive license and it would
be better to avoid using this proprietary program.  I encourage people
to write a free software alternative rather than using this tool."))))

(define-public bcl2fastq-latest
  (package (inherit bcl2fastq)
    (version "2.20.0")
    (source (origin
              (method url-fetch)
              ;; Download manually from here:
              ;; ftp://webdata2:webdata2@ussd-ftp.illumina.com/downloads/software/bcl2fastq/bcl2fastq2-v2-20-0-tar.zip
              (uri (string-append "file:///gnu/remote/bcl2fastq2-v"
                                  (string-join (string-split version #\.) "-")
                                  "-tar.zip"))
              (sha256
               (base32
                "1qqz217ipsv5wq28wd5pp3jl870i5dbdxq3dwi6ali6hcx3h9lwd"))))
    (arguments
     (substitute-keyword-arguments (package-arguments bcl2fastq)
       ((#:configure-flags flags)
        `(list (string-append "-DBCL2FASTQ_VERSION:STRING=" ,version)
               "-DBCL2FASTQ_NAME_SHORT:STRING=bcl2fastq"
               "-DBCL2FASTQ_NAME_LONG:STRING=BCL to FASTQ file converter"
               "-DBCL2FASTQ_COPYRIGHT:STRING=Copyright (c) 2007-2018 Illumina, Inc."
               (string-append "-DBCL2FASTQ_SOURCE_DIR:STRING=" (getcwd) "/bcl2fastq/src")))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'unpack
             (lambda* (#:key source #:allow-other-keys)
               (invoke "unzip" source)
               (invoke "tar" "-xvf"
                       "bcl2fastq2-v2.20.0.422-Source.tar.gz")
               (substitute* "bcl2fastq/src/cxx/include/common/Logger.hh"
                 (("#include <ios>" m)
                  (string-append m "\n#include <iostream>")))
               #t))
           (add-after 'install 'rename-/bin/test
             (lambda* (#:key outputs #:allow-other-keys)
               (rename-file (string-append (assoc-ref outputs "out") "/bin/test")
                            (string-append (assoc-ref outputs "out") "/bin/bcl2fastq-test"))
               #t))))))))

(define-public bcl2fastq1
  (package (inherit bcl2fastq)
    (name "bcl2fastq1")
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              ;; Download manually from here:
              ;; ftp://webdata:webdata@ussd-ftp.illumina.com/Downloads/Software/bcl2fastq/bcl2fastq-1.8.4.tar.bz2
              (uri (string-append "file:///gnu/remote/bcl2fastq-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "14s15h8kk9vqqwy0hykdzffz6zlkbqpvg5wnnfiwd2x7cwxizikm"))
              (snippet
               `(begin
                  (delete-file "redist/boost_1_44_0.tar.gz")
                  (delete-file "redist/cmake-2.8.4.tar.gz")))))
    (arguments
     `(#:configure-flags
       ,#~(list (string-append "-DBOOST_ROOT="
			                   #$(this-package-input "boost"))
	            "-DBoost_DEBUG=ON"
	            ;; Needed for later versions of CMake with older versions of Boost
	            "-DBoost_NO_BOOST_CMAKE=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'hide-default-gcc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               ;; Remove the default GCC from CPLUS_INCLUDE_PATH to prevent
               ;; conflicts with the GCC 5 input.
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                        ":")))))
         (add-after 'unpack 'enter-dir (lambda _ (chdir "src")))
         (add-after 'enter-dir 'fix-includes
           (lambda _
             (substitute* "c++/include/common/FileConversion.hh"
               (("#pragma once" line)
                (string-append line "\n#include <stdint.h>")))
             (substitute* "c++/include/demultiplex/BarcodeTranslationTable.hh"
               (("^namespace casava" line)
                (string-append "#include <stdint.h>\n" line)))))
         (add-after 'install 'wrap-perl-scripts
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; Make sure perl scripts finds all perl inputs at runtime.
            (let ((out (assoc-ref outputs "out")))
              (for-each (lambda (prog)
                          (wrap-program (string-append out "/bin/" prog)
                            `("PERL5LIB" ":" prefix
                              (,(getenv "PERL5LIB")))))
                        '("configureBclToFastq.pl"
                          "configureQseqToFastq.pl"
                          "configureValidation.pl"))))))))
    (native-inputs
     `(("gcc@4" ,gcc-4.9)))
    (inputs
     `(;; We need the older version of Boost although this could be
       ;; built with 1.55 with only minor changes.  The reason is
       ;; option parsing, which only bites us at runtime.
       ("boost" ,boost-1.44)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("perl-xml-simple" ,(package-for-other-perl perl-5.14 perl-xml-simple))
       ("perl-xml-parser" ,(package-for-other-perl perl-5.14 perl-xml-parser))
       ("perl" ,perl-5.14)
       ("zlib" ,zlib)))))

(define-public dinup
  (package
    (name "dinup")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://zhanglab.tongji.edu.cn/softwares/DiNuP/dinup_"
                    version ".tar.gz"))
              (sha256
               (base32
                "14s15h8kk9vqqwy0hykdzffz6zlkbqpvg5wnnfiwd2x7cwxizikm"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no "test" target
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           ;; The release tarball contains loose files.
           (lambda* (#:key source #:allow-other-keys)
             (mkdir "dinup")
             (invoke "tar" "-C" "dinup" "-xvf" source)
             (chdir "dinup"))))))
    (native-inputs
     (list python2-setuptools))
    (home-page "http://zhanglab.tongji.edu.cn/softwares/DiNuP/")
    (synopsis "Identify regions of differential nucleosome positioning")
    (description
     "DiNuP compares the nucleosome profiles generated by high-throughput
sequencing between different conditions.  DiNuP provides a statistical p-value
for each identified region of differential nucleosome positioning (RDNP) based
on the difference of read distributions.  It also empirically estimates the
false discovery rate as a cutoff when two samples have different sequencing
depths and differentiate reliable RDNPs from the background noise.")
    (license nonfree:artistic1.0)))

(define-public macs-1
  (package (inherit macs)
    (name "macs")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pypi.python.org/packages/source/M/MACS/MACS-"
                    version ".tar.gz"))
              (sha256
               (base32
                "17lbf76gkisrxhnjwf8iw4pvinny2376dp9dyrgald2l0ww6s4d9"))
              (patches (list (search-patch "macs-1.4-fix-parser.patch")))))
    (build-system python-build-system)
    (arguments `(#:tests? #false #:python ,python-2))
    (inputs '())
    (native-inputs '())
    (license nonfree:artistic1.0)))

(define-public fstitch
  (let ((commit "7c65fd973f1d04d83cd48dd5561c4e40c14dd8c6")
        (revision "1"))
    (package
      (name "fstitch")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/azofeifa/FStitch.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "0g00hdc73w68big3prym0llx0nl7w7xhbfp6g405yfn7dghc8v8c"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Delete pre-built binaries
                    (delete-file "FastReadStitcher/src/FStitch")
                    (for-each delete-file
                              (find-files "FastReadStitcher/src/" "\\.o$"))))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ; no tests included
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'enter-dir
              (lambda _
                (chdir "FastReadStitcher/src/")
                (substitute* "Makefile"
                  (("\\$\\{PWD\\}/") ""))))
            (delete 'configure)
            (replace 'install
              (lambda _
                (let ((bin (string-append #$output "/bin")))
                  (mkdir-p bin)
                  (install-file "FStitch" bin)))))))
      (home-page "https://github.com/azofeifa/FStitch/")
      (synopsis "Detect nascent RNA transcription in GRO-seq and ChIP-seq")
      (description
       "FStitch was written primarily for scientists looking to
identify putative nascent transcripts de novo in Global Run-On
sequencing data.  However, users may also find this package useful as
a ChIP-seq peak caller.")
      (license nonfree:undeclared))))

(define-public meme
  (package
    (name "meme")
    (version "4.11.3_1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://meme-suite.org/meme-software/"
                                  (car (string-split version #\_))
                                  "/meme_" version ".tar.gz"))
              (sha256
               (base32
                "08df4wgiz1baq3749slpmr7df0hg3q4i3cdvap97xw063kx2d9gc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths-to-tools
            (lambda _
              (substitute* "src/utils.c"
                (("\"hostname")
                 (string-append "\"" (which "hostname"))))))
          (add-after 'unpack 'remove-unused-tests
            (lambda _
              ;; We don't build the web server stuff, so we don't need
              ;; to run the tests for that either.
              (substitute* "tests/scripts/Makefile.in"
                (("tomtom.test") ""))))
          (add-before 'configure 'check-perl-dependencies
            (lambda _
              (invoke "perl" "./scripts/dependencies.pl")))
          (add-after 'install 'wrap-perl-scripts
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Make sure perl scripts finds all perl inputs at runtime.
              (for-each (lambda (prog)
                          (wrap-program (string-append #$output "/bin/" prog)
                            `("PERL5LIB" ":" prefix
                              (,(getenv "PERL5LIB")))))
                        '("ama-qvalues"
                          "beeml2meme"
                          "chen2meme"
                          "dreme_xml_to_html"
                          "dreme_xml_to_txt"
                          "elm2meme"
                          "fasta-center"
                          "fasta-fetch"
                          "fasta-grep"
                          "fasta-make-index"
                          "fasta-most"
                          "fasta-subsample"
                          "fasta-unique-names"
                          "hart2meme-bkg"
                          "hartemink2psp"
                          "iupac2meme"
                          "jaspar2meme"
                          "mast_xml_to_html"
                          "mast_xml_to_txt"
                          "matrix2meme"
                          "meme-chip"
                          "meme-rename"
                          "meme_xml_to_html"
                          "nmica2meme"
                          "priority2meme"
                          "psp-gen"
                          "rna2meme"
                          "rsat-retrieve-seq"
                          "rsat-supported-organisms"
                          "scpd2meme"
                          "sites2meme"
                          "taipale2meme"
                          "tamo2meme"
                          "tomtom_xml_to_html"
                          "transfac2meme"
                          "uniprobe2meme")))))))
    (native-inputs (list gcc-4.9))
    (inputs
     (list perl
           perl-file-which
           perl-html-parser
           perl-html-template
           perl-xml-simple
           perl-xml-compile
           perl-xml-compile-wsdl11
           perl-xml-parser
           python-2                     ;only works with Python 2
           libxml2
           libxslt
           openmpi
           ghostscript
           inetutils                    ;for "hostname"
           zlib))
    (propagated-inputs
     ;; "which" must be propagated because of the weird way it is used
     ;; in "src/exec_parallel.c".  The buffer "cmd_len" is arranged to
     ;; be 6 characters longer than the argument, just enough for the
     ;; string "which ".  I don't want to mess with pointers and
     ;; buffer lengths just to hardcode a path to the "which"
     ;; executable.
     (list which))
    (home-page "http://www.tbi.univie.ac.at/RNA/index.html")
    (synopsis "Motif-based sequence analysis tools")
    (description
     "The MEME Suite allows the biologist to discover novel motifs in
collections of unaligned nucleotide or protein sequences, and to
perform a wide variety of other motif-based analyses.

The MEME Suite supports motif-based analysis of DNA, RNA and protein
sequences.  It provides motif discovery algorithms using both
probabilistic and discrete models, which have complementary strengths.
It also allows discovery of motifs with arbitrary insertions and
deletions (GLAM2).  In addition to motif discovery, the MEME Suite
provides tools for scanning sequences for matches to motifs (FIMO,
MAST and GLAM2Scan), scanning for clusters of motifs (MCAST),
comparing motifs to known motifs (Tomtom), finding preferred spacings
between motifs (SpaMo), predicting the biological roles of
motifs (GOMo), measuring the positional enrichment of sequences for
known motifs (CentriMo), and analyzing ChIP-seq and other large
datasets (MEME-ChIP).")
    (license (nonfree:non-free "http://meme-suite.org/doc/copyright.html"
                               "license forbids commercial usage"))))

(define-public structure
  (package
    (name "structure")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pritchardlab.stanford.edu/"
                           "structure_software/release_versions/v" version
                           "/structure_kernel_source.tar.gz"))
       (sha256
        (base32
         "0dxvq34lyzicjwgsyrw49b1pmjms7nmc3g8vj8zga555i68jpdzj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; There are no tests.
      #:make-flags '(list "CFLAGS=-fcommon")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ; There is no configure phase.
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (install-file "structure" bin)))))))
    (home-page "http://pritchardlab.stanford.edu/structure.html")
    (synopsis "Tool for investigating population structure")
    (description "Structure is a package for using multi-locus genotype data
to investigate population structure.  Its uses include inferring the presence
of distinct populations, assigning individuals to populations, studying hybrid
zones, identifying migrants and admixed individuals, and estimating population
allele frequencies in situations where many individuals are migrants or
admixed.  It can be applied to most of the commonly-used genetic markers,
including SNPS, microsatellites, RFLPs and AFLPs.")
    ;; I have asked upstream for information about the license:
    ;; https://groups.google.com/forum/#!topic/structure-software/1g7bDoN9140
    (license nonfree:undeclared)))

(define-public nofold
  (let ((revision "1")
        (commit "a3da753118db8310d453669aa01d34a270532a4b"))
    (package
      (name "nofold")
      (version (string-append "0.0.0-"
                              revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sarahmid/nofold.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "0fq33ra4nrnyjvwd4vc9r2mxrdihkb5imwms7b2kl6dr76vfmy1z"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (delete 'build)
            (replace 'install
              (lambda _
                (let ((target (string-append #$output "/share/nofold")))
                  (copy-recursively "." target)))))))
      (inputs
       (list python-2
             locarna
             infernal-1.0
             r
             r-fastcluster))
      (synopsis "Motif finder for RNA secondary structures")
      (description
       "NoFold is an approach for characterizing and clustering RNA
secondary structures without computational folding or alignment.  It
works by mapping each RNA sequence of interest to a structural feature
space, where each coordinate within the space corresponds to the
probabilistic similarity of the sequence to an empirically defined
structure model (e.g. Rfam family covariance models).  NoFold provides
scripts for mapping sequences to this structure space, extracting any
robust clusters that are formed, and annotating those clusters with
structural and functional information.")
      (home-page "https://github.com/sarahmid/nofold")
      (license (nonfree:non-free "https://raw.githubusercontent.com/sarahmid/nofold/master/LICENSE"
                                 "license forbids commercial usage")))))

(define-public defuse-tools
  (package
    (name "defuse-tools")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/dranew/defuse/"
                           "get/v" version ".tar.bz2"))
       (sha256
        (base32
         "0my24iw5mqdrq1k08casdv5p4wvk01inikhvi8mb74r7z93kcpf5"))
       (modules '((guix build utils)))
       ;; Delete bundled stuff
       (snippet '(delete-file-recursively "external"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no check target
      #:make-flags
      #~(list "ZLIB=-lz"
              (string-append "BAMTOOLSSRC="
                             #$(this-package-input "bamtools")
                             "/lib/bamtools/libbamtools.a")
              (string-append "BAMTOOLSDIR="
                             #$(this-package-input "bamtools")
                             "/include/bamtools"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'chdir
            (lambda _ (chdir "tools")))
          (add-after 'chdir 'do-not-use-bundled-libraries
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "makefile"
                (("\\$\\(BAMDIR\\)/\\$\\(BAMLIB\\)")
                 (string-append (assoc-ref inputs "samtools")
                                "/lib/libbam.a"))
                (("\\$\\(ZDIR\\)/\\$\\(ZLIB\\)") "$(ZLIB)")
                ;; Don't try to build the bundled libs
                (("\\$\\(MAKE\\).*") ""))
              (substitute* '("FastaIndex.cpp"
                             "FastaIndex.h")
                (("#include \"faidx.h\"")
                 "#include <samtools/faidx.h>"))
              (substitute* "bamfastq.cpp"
                (("#include \"BamReader.h\"")
                 "#include <bamtools/api/BamReader.h>"))))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (for-each
                 (lambda (file) (install-file file bin))
                 '("clustermatepairs"
                   "setcover"
                   "calccov"
                   "estislands"
                   "dosplitalign"
                   "evalsplitalign"
                   "localalign"
                   "splitseq"
                   "matealign"
                   "bamfastq"))))))))
    (inputs
     (list zlib
           boost
           bamtools-2.0
           samtools-0.1))
    (home-page "https://bitbucket.org/dranew/defuse")
    (synopsis "Gene fusion discovery using RNA-Seq data")
    (description "deFuse is a software package for gene fusion
discovery using RNA-Seq data.  The software uses clusters of
discordant paired end alignments to inform a split read alignment
analysis for finding fusion boundaries.  The software also employs a
number of heuristic filters in an attempt to reduce the number of
false positives and produces a fully annotated output for each
predicted fusion.

This package provides only the binaries from the \"tools\" directory,
not the pipeline scripts.")
    (license (nonfree:non-free "file://LICENSE.md"))))

(define-public gmap-gsnap
  (package
    (name "gmap-gsnap")
    (version "2017-08-15")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://research-pub.gene.com/gmap/src/gmap-gsnap-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0bqvv26g4ic3nmrcdnnh7kqly86ly9gym75nw1wyzav4ad5h8rqv"))))
    (build-system gnu-build-system)
    ;; FIXME: there are test failures, but I'm not sure why.
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; Because of multiple definitions.
         (add-after 'unpack 'set-fcommon
           (lambda _ (setenv "CFLAGS" "-fcommon"))))))
    (inputs (list perl))
    (home-page "http://research-pub.gene.com/gmap/")
    (synopsis "Genomic mapper and aligner")
    (description "This package provides GMAP and GSNAP.  The former is
a genomic mapping and alignment program for mRNA and EST sequences;
the latter is an aligner for genomic short-read nucleotide sequences.")
    (license (nonfree:non-free "Distribution of modified versions
requires the author's consent."))))

(define-public music
  (let ((commit "6613c532bf21d958ae380f064cc7752f3e8c368f")
        (revision "1"))
    (package
      (name "music")
      (version (string-append "0.0.0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gersteinlab/MUSIC.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1in3s6qimbx5s10sxx943krj52702s2f7yk9nfr54z5yirwqrf36"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ; no "check" target
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            ;; There is no "install" target.
            (replace 'install
              (lambda _
                (let ((bin (string-append #$output "/bin")))
                  (mkdir-p bin)
                  (install-file "bin/MUSIC" bin)))))))
      (home-page "http://music.gersteinlab.org")
      (synopsis "Multiscale enrichment calling for ChIP-Seq datasets")
      (description
       "MUSIC is an algorithm for identification of enriched regions
at multiple scales in the read depth signals from ChIP-Seq
experiments.")
      ;; https://github.com/gersteinlab/MUSIC/issues/6
      (license nonfree:undeclared))))

(define-public cufflinks
  ;; This commit includes build fixes
  (let ((commit "dc3b0cb72a4ac2b6bbc887099e71fc0c21e107b7")
        (revision "1"))
    (package
      (name "cufflinks")
      (version (git-version "2.2.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/cole-trapnell-lab/cufflinks")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1mgmakzg4g174ry0idyzjwysssb5ak72ybk0qha8vi6raylhd3if"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags
        #~(list
           ;; Cufflinks must be linked with various boost libraries.
           (string-append "BOOST_LDFLAGS=-L"
                          #$(this-package-input "boost")
                          "/lib"))
         #:phases
         '(modify-phases %standard-phases
            (add-after 'unpack 'fix-library-detection
              (lambda _
                (substitute* "ax_boost_system.m4"
                  (("BOOSTLIBDIR/boost_system")
                   "BOOSTLIBDIR/libboost_system"))
                ;; The includes for "eigen" are located in a subdirectory.
                (substitute* "ax_check_eigen.m4"
                  (("ac_eigen_path/include")
                   "ac_eigen_path/include/eigen3")))))
         #:configure-flags
         #~(cons* (string-append "--with-bam="
                                 #$(this-package-input "htslib"))
                  (string-append "--with-eigen="
                                 #$(this-package-input "eigen"))
                  (map (lambda (lib)
                         (string-append "--with-boost-" lib "=boost_" lib))
                       '("system"
                         "filesystem"
                         "serialization"
                         "thread")))))
      (inputs
       (list eigen
             htslib
             boost-1.68              ;latest boost doesn't allow c++03
             python-2
             zlib))
      (native-inputs
       (list autoconf
             automake
             gcc-7))
      (home-page "http://cole-trapnell-lab.github.io/cufflinks/")
      (synopsis "Transcriptome assembly and RNA-Seq expression analysis")
      (description
       "Cufflinks assembles RNA transcripts, estimates their abundances,
and tests for differential expression and regulation in RNA-Seq
samples.  It accepts aligned RNA-Seq reads and assembles the
alignments into a parsimonious set of transcripts.  Cufflinks then
estimates the relative abundances of these transcripts based on how
many reads support each one, taking into account biases in library
preparation protocols.")
      ;; The sources include a modified third-party library "locfit"
      ;; that is released under a license asking any use for benchmarks
      ;; to be authorized.  A GPL version of locfit (by the same author)
      ;; exists in the form of "r-locfit", but it cannot be used without
      ;; modifications.
      ;; See also https://github.com/cole-trapnell-lab/cufflinks/issues/129
      (license (list license:boost1.0
                     (nonfree:non-free "Not to be used for benchmarks"))))))

(define-public perl-math-cdf
  (package
    (name "perl-math-cdf")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/C/CA/CALLAHAN/Math-CDF-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0ram2brgxlyqszf25s22vram8v2pvkwqrjqkr3f4gkim10jvz5kq"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Math-CDF/")
    (synopsis "Generate probabilities and quantiles from several statistical probability functions")
    (description "This module provides a perl interface to the
DCDFLIB.  Functions are available for 7 continuous
distributions (Beta, Chi-square, F, Gamma, Normal, Poisson and
T-distribution) and for two discrete distributions (Binomial and
Negative Binomial).  Optional non-centrality parameters are available
for the Chi-square, F and T-distributions.  Cumulative probabilities
are available for all 9 distributions and quantile functions are
available for the 7 continuous distributions.")
    ;; Parts of the original library are public domain.  Others are
    ;; non-commercial only.  It is unclear which is which.
    (license nonfree:undeclared)))

;; For harm
(define-public perl5.24-math-cdf
  (package-for-other-perl perl-5.24 perl-math-cdf))

(define-public rsat
  (package
    (name "rsat")
    (version "2018-06-07")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "http://pedagogix-tagc.univ-mrs.fr/"
                                        "download_rsat/rsat_"
                                        version ".tar.gz")
                         (string-append "http://pedagogix-tagc.univ-mrs.fr/"
                                        "download_rsat/previous_versions/rsat_"
                                        version ".tar.gz")))
              (sha256
               (base32
                "0zzlfl19ylj06np8hhmkjavv7906nk58ngg5irc37jr03p346l3x"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no test target
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              ;; Ensure that we can override the target directory
              (substitute* "perl-scripts/configure_rsat.pl"
                (("\\$rsat_parent_path = .*;")
                 (string-append "$rsat_parent_path = \""
                                #$output "\";")))
              ;; Override the target directory
              (setenv "RSAT" #$output)
              ;; Target directory must exist
              (mkdir-p "bin")
              (invoke "perl" "perl-scripts/configure_rsat.pl")))
          (replace 'build
            (lambda _
              ;; FIXME: this first step is pretty useless, because it
              ;; creates directories not in the install location but in
              ;; the build directory.
              (invoke "make" "-f" "makefiles/init_rsat.mk" "init")
              (invoke "make" "-f" "makefiles/init_rsat.mk" "compile_all")))
          (replace 'install
            (lambda _
              (copy-recursively "bin" (string-append #$output "/bin")))))))
    (native-inputs
     (list perl))
    (home-page "http://rsat.eead.csic.es/plants/")
    (synopsis "Regulatory sequence analysis tools")
    (description "This package provides a subset of the Regulatory
Sequence Analysis Tools (RSAT).")
    (license (nonfree:non-free
              "The stand-alone version is freely available for academic
users, with some restrictions on utilization (non-commercial,
non-military and non-redistribution)."))))

(define-public r-cellalign
  (let ((commit "30fb085bcf45280e1706450e9cfcba3728cda6d8")
        (revision "1"))
    (package
      (name "r-cellalign")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                 (url "https://github.com/shenorrLabTRDF/cellAlign")
                 (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                  "1v0ci71zbrmr1l2xmljdl6ks508fmhh6sdc3iglgd3cj16d0ahj6"))))
      (properties `((upstream-name . "cellAlign")))
      (build-system r-build-system)
      (propagated-inputs
        (list r-dtw
              r-ggplot2
              r-pheatmap
              r-rcpp
              r-reshape2))
      (home-page "https://github.com/shenorrLabTRDF/cellAlign")
      (synopsis "Global and local alignment of single cell trajectories")
      (description
       "More about what it does (maybe more than one line) Use four spaces when
indenting paragraphs within the Description.")
      (license (nonfree:non-free
                    "The stand-alone version is freely available for academic
users, with no restrictions beside usit \"for-profit\".  The users who intend
to optain revenues by using this software are needed to buy a license.")))))

(define-public r-loomr
  (let ((commit "df0144bd2bbceca6fadef9edc1bbc5ca672d4739")
        (revision "1"))
    (package
      (name "r-loomr")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mojaveazure/loomR.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1b1g4dlmfdyhn56bz1mkh9ymirri43wiz7rjhs7py3y7bdw1s3yr"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-r6
             r-hdf5r
             r-iterators
             r-itertools
             r-matrix))
      (home-page "https://github.com/mojaveazure/loomR")
      (synopsis "R interface for loom files")
      (description "This package provides an R interface to access, create,
and modify loom files.  loomR aims to be completely compatible with loompy.")
      ;; The DESCRIPTION file says GPL-3, but according to the author
      ;; they haven't decided an a final license yet.  For now we
      ;; should consider it licensed under the non-free DBAD license.
      (license (nonfree:non-free "https://dbad-license.org")))))

;; The latest release is from 2019 and still at version 1.1.2.
(define-public r-scenic
  (let ((commit "0585e873d7f17d35e7172f6f3e7e369f9820ef14")
        (revision "1"))
    (package
      (name "r-scenic")
      (version (git-version "1.2.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aertslab/SCENIC")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0zkwffx40wxkv0zg8lnzr6m4j8zjkjs26dgbq1pzvnaxj5j6gbnl"))))
      (properties `((upstream-name . "SCENIC")))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-aucell" ,r-aucell)
         ("r-data-table" ,r-data-table)
         ("r-dynamictreecut" ,r-dynamictreecut)
         ("r-genie3" ,r-genie3)
         ("r-mixtools" ,r-mixtools)
         ("r-nmf" ,r-nmf)
         ("r-rcistarget" ,r-rcistarget)
         ("r-rtsne" ,r-rtsne)
         ("r-singlecellexperiment" ,r-singlecellexperiment)))
      (home-page "https://github.com/aertslab/SCENIC")
      (synopsis "Single cell regulatory network inference and clustering")
      (description "SCENIC (Single-cell regulatory network inference
and clustering) is an R package to infer Gene Regulatory Networks and
cell types from single-cell RNA-seq data.")
      (license (nonfree:non-free "Non-commercial")))))

(define-public cermit
  (package
    (name "cermit")
    (version "1.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "file:///gnu/remote/cermit-" version ".tar.gz"))
              (sha256
               (base32
                "18p06n4ziykiznbk4b1fzjyg2cm43nqr82g6q6p14jym6lywgi5d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags '("cERMIT")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "source/cERMIT") #t))
         (add-after 'chdir 'patch-references
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "motif_finder.c"
               (("command, \"mv")
                (string-append "command, \"" (which "mv"))))
             (substitute* "PSSM.c"
               (("command, \"R")
                (string-append "command, \"" (which "R")))
               (("./generate_logo.R")
                (string-append (assoc-ref outputs "out")
                               "/share/cermit/generate_logo.R")))
             #t))
         (add-after 'chdir 'fix-makefile
           (lambda _
             (substitute* "makefile"
               (("-m32") "")
               (("../libs/libgsl_1.14.a ../libs/libgslcblas_1.14.a")
                "-lgsl -lgslcblas"))
             (mkdir "bin")
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share/cermit")))
               (mkdir-p bin)
               (install-file "bin/cERMIT" bin)

               (mkdir-p share)
               (install-file "../../generate_logo.R" share)
               #t))))))
    (inputs
     `(("gsl" ,gsl-1.16)
       ("r" ,r-minimal)))
    (supported-systems '("i686-linux"))
    (home-page "TODO")
    (synopsis "TODO")
    (description "TODO")
    ;; It should be GPLv3+ because it uses GSL and includes other code
    ;; under the GPLv3+.
    (license nonfree:undeclared)))

;; This perl script has no license declared but contains an 'All Rights
;; Reserved' Copyright statement.
(define-public targetscan
  (package
    (name "targetscan")
    (version "7.0")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                     "http://www.targetscan.org/vert_72"
                     "/vert_72_data_download/targetscan_70.zip"))
              (sha256
               (base32
                "0jfpkc0rbz600l2h56rmnnz6phcvjak37zl3yzn5hahgj8mcf8vf"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("targetscan_70.pl" "bin/")
         ("README_70.txt" "doc/targetscan_70/")
         ("targetscan_70_output.txt" "share/targetscan_70/")
         ("UTR_Sequences_sample.txt" "share/targetscan_70/")
         ("miR_Family_info_sample.txt" "share/targetscan_70/"))))
    (home-page "http://www.targetscan.org")
    (native-inputs (list unzip))
    (inputs (list perl))
    (synopsis "This program predicts miRNA targets using the TargetScanS
algorithm.")
    (description "It produces output as displayed in TargetScan.")
    (license (nonfree:non-free "All Rights Reserved"))))
