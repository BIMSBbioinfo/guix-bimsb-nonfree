;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <ricardo.wurmus@mdc-berlin.de>
;;; Copyright © 2017 CM Massimo <carlomaria.massimo@mdc-berlin.de>
;;; Copyright © 2018 Marcel Schilling <marcel.schilling@mdc-berlin.de>
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
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
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
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (bimsb packages staging)
  #:use-module (bimsb packages variants))

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
            (and (zero? (system* "unzip" source))
                 (zero? (system* "tar" "-xvf"
                                 (string-append "bcl2fastq2-v"
                                                ,version ".tar.gz"))))))
         (add-after 'unpack 'enter-dir (lambda _ (chdir "bcl2fastq/src") #t))
         (add-after 'enter-dir 'patch-stuff
                    (lambda _
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
                         (string-append (assoc-ref %build-inputs "libxslt")
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
                                         (package-version libxslt))))
                      #t)))))
    (inputs
     `(("boost" ,boost)
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
                  (delete-file "redist/cmake-2.8.4.tar.gz")
                  #t))))
    (arguments
     `(#:configure-flags
       (list (string-append "-DBOOST_ROOT="
			    (assoc-ref %build-inputs "boost"))
	     "-DBoost_DEBUG=ON"
	     ;; Needed for later versions of CMake with older versions of Boost
	     "-DBoost_NO_BOOST_CMAKE=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir (lambda _ (chdir "src") #t))
         (add-after 'enter-dir 'fix-includes
           (lambda _
             (substitute* "c++/include/common/FileConversion.hh"
               (("#pragma once" line)
                (string-append line "\n#include <stdint.h>")))
             (substitute* "c++/include/demultiplex/BarcodeTranslationTable.hh"
               (("^namespace casava" line)
                (string-append "#include <stdint.h>\n" line)))
             #t))
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
                          "configureValidation.pl"))
              #t))))))
    (native-inputs
     `(("gcc" ,gcc-4.9)))
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

(define-public bowtie1
  (package
    (inherit bowtie)
    (name "bowtie1")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bowtie-bio/bowtie/"
                                  version "/bowtie-" version "-src.zip"))
              (sha256
               (base32
                "1bipnvm94nlzbawix09bkfdvig41qr235qyrjccgszi04p4crsdi"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile"
                  ;; replace BUILD_HOST and BUILD_TIME for deterministic build
                  (("-DBUILD_HOST=.*") "-DBUILD_HOST=\"\\\"guix\\\"\"")
                  (("-DBUILD_TIME=.*") "-DBUILD_TIME=\"\\\"0\\\"\"")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no "check" target
       #:make-flags
       (list "all"
             "WITH_TBB=0" ; doesn't build with the latest TBB
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://bowtie-bio.sourceforge.net/index.shtml")
    (synopsis "Fast aligner for short nucleotide sequence reads")
    (description
     "Bowtie is a fast, memory-efficient short read aligner.  It aligns short
DNA sequences (reads) to the human genome at a rate of over 25 million 35-bp
reads per hour.  Bowtie indexes the genome with a Burrows-Wheeler index to
keep its memory footprint small: typically about 2.2 GB for the human
genome (2.9 GB for paired-end).")
    (supported-systems '("x86_64-linux"))
    (license nonfree:artistic1.0)))

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
       #:phases (alist-replace
                 'unpack
                 ;; The release tarball contains loose files.
                 (lambda* (#:key source #:allow-other-keys)
                   (and (mkdir "dinup")
                        (zero? (system* "tar" "-C" "dinup" "-xvf" source))
                        (chdir "dinup")))
                 %standard-phases)))
    (native-inputs
     `(("python-setuptools" ,python2-setuptools)))
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
              (patches (list (search-patch "macs-1.4-fix-parser.patch")))))))

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
                              (find-files "FastReadStitcher/src/" "\\.o$"))
                    #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests included
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-dir
             (lambda _
               (chdir "FastReadStitcher/src/")
               (substitute* "Makefile"
                 (("\\$\\{PWD\\}/") ""))
               #t))
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out")
                                         "/bin")))
                 (mkdir-p bin)
                 (install-file "FStitch" bin)
                 #t))))))
      (home-page "https://github.com/azofeifa/FStitch/")
      (synopsis "Detect nascent RNA transcription in GRO-seq and ChIP-seq")
      (description
       "FStitch was written primarily for scientists looking to
identify putative nascent transcripts de novo in Global Run-On
sequencing data.  However, users may also find this package useful as
a ChIP-seq peak caller.")
      (license nonfree:undeclared))))

(define-public python2-mirnylib
  (let ((commit "ccec2e72dfa33eb04fe8b2ebd9bc2d88a1776d63")
        (revision "2"))
    (package
      (name "python2-mirnylib")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://bitbucket.org/mirnylab/mirnylib")
                      (changeset commit)))
                (sha256
                 (base32
                  "08ac4jg6bz4528x7sbnybkvjhk1w7jigmbl30qh4qlp6jfhf03bk"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2 ; python2 only
         #:tests? #f ; tests expect additional test data
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'use-distutils
            (lambda _
              (substitute* "setup.py"
                (("from setuptools import setup")
                 "from distutils.core import setup"))
              #t)))))
      (inputs
       `(("gcc" ,gcc "lib")    ; libgomp
         ("hdf5" ,hdf5)))      ; FIXME: probably should be propagated by h5py
      (propagated-inputs
       `(("python-biopython" ,python2-biopython)
         ("python-joblib" ,python2-joblib)
         ("python-bx-python" ,python2-bx-python)
         ("python-numpy" ,python2-numpy)
         ("python-scipy" ,python2-scipy)
         ("python-pysam" ,python2-pysam)
         ("python-matplotlib" ,python2-matplotlib)
         ("python-h5py" ,python2-h5py)))
      (native-inputs
       `(("python-cython" ,python2-cython)
         ("python-setuptools" ,python2-setuptools)))
      (home-page "https://bitbucket.org/mirnylab/mirnylib")
      (synopsis "Libraries shared between different mirnylab projects")
      (description
       "This package provides assorted libraries used by different mirnylab
projects.")
      (license nonfree:undeclared))))

;; https://bitbucket.org/mirnylab/hiclib/issues/36/no-license-declaration
(define-public python2-hiclib
  (let ((commit "1193891")
        (revision "2"))
    (package
      (name "python2-hiclib")
      (version (string-append "0-" revision "." commit))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://bitbucket.org/mirnylab/hiclib")
                      (changeset commit)))
                (sha256
                 (base32
                  "18y46bic26ya7zna9kakfifgqf6r4q9395nsjwv5hzcclfk755nf"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; tests depend on unavailable test data
         #:python ,python-2 ; python2 only
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'use-distutils
            (lambda _
              ;; HOME needs to be set to unpack the Egg archive
              (setenv "HOME" "/tmp")
              (substitute* "setup.py"
                (("from setuptools import setup")
                 "from distutils.core import setup"))
              #t))
           (add-after 'unpack 'set-matplotlib-backend-to-agg
            (lambda _
              ;; Set the matplotlib backend to Agg to avoid problems using the
              ;; GTK backend without a display.
              (substitute* (find-files "tests" "\\.py")
                (("import matplotlib\\.pyplot as plt" line)
                 (string-append "import matplotlib;matplotlib.use('Agg');"
                                line)))
              #t))
           (add-before 'build 'build-binarySearch
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((python-version (string-take (string-take-right
                                                   (assoc-ref inputs "python") 6) 3))
                     (path (string-append "lib/python" python-version
                                          "/site-packages")))
                (substitute* "setup.py"
                  (("binarySearch/fastBinSearch.pyx")
                   "binarySearch/fastBinSearch.cpp")
                  (("packages=\\['hiclib'\\],")
                   (string-append "packages=['hiclib'], "
                                  "data_files=[('" path "/hiclib', "
                                  "['binarySearch/fastBinSearch.so'])],")))
                (with-directory-excursion "binarySearch"
                  (setenv "CPATH"
                          (string-append (assoc-ref inputs "python-numpy")
                                         "/" path "/numpy/core/include/:"
                                         (or (getenv "CPATH") "")))
                  (zero? (system* "make")))))))))
      (propagated-inputs
       `(("hdf5" ,hdf5) ; FIXME: probably should be propagated by h5py
         ("python-biopython" ,python2-biopython)
         ("python-numpy" ,python2-numpy)
         ("python-scipy" ,python2-scipy)
         ("python-matplotlib" ,python2-matplotlib)
         ("python-pysam" ,python2-pysam)
         ("python-mirnylib" ,python2-mirnylib)))
      (native-inputs
       `(("python-cython" ,python2-cython)
         ("python-setuptools" ,python2-setuptools)))
      (home-page "https://bitbucket.org/mirnylab/hiclib")
      (synopsis "Collection of tools to map, filter and analyze Hi-C data")
      (description
       "Hi-C lib is a collection of tools to map, filter and analyze Hi-C
data.")
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
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths-to-tools
           (lambda _
             (substitute* "src/utils.c"
               (("\"hostname")
                (string-append "\"" (which "hostname"))))
             #t))
         (add-after 'unpack 'remove-unused-tests
           (lambda _
             ;; We don't build the web server stuff, so we don't need
             ;; to run the tests for that either.
             (substitute* "tests/scripts/Makefile.in"
               (("tomtom.test") ""))))
         (add-before 'configure 'check-perl-dependencies
           (lambda _
             (zero? (system* "perl" "./scripts/dependencies.pl"))))
         (add-after 'install 'wrap-perl-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure perl scripts finds all perl inputs at runtime.
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (prog)
                           (wrap-program (string-append out "/bin/" prog)
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
                           "uniprobe2meme"))
              #t))))))
    (inputs
     `(("perl" ,perl)
       ("perl-file-which" ,perl-file-which)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-html-template" ,perl-html-template)
       ("perl-xml-simple" ,perl-xml-simple)
       ("perl-xml-compile" ,perl-xml-compile)
       ("perl-xml-compile-wsdl11" ,perl-xml-compile-wsdl11)
       ("perl-xml-parser" ,perl-xml-parser)
       ("python" ,python-2) ;only works with Python 2
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("openmpi" ,openmpi)
       ("ghostscript" ,ghostscript)
       ("inetutils" ,inetutils) ;for "hostname"
       ("zlib" ,zlib)))
    (propagated-inputs
     ;; "which" must be propagated because of the weird way it is used
     ;; in "src/exec_parallel.c".  The buffer "cmd_len" is arranged to
     ;; be 6 characters longer than the argument, just enough for the
     ;; string "which ".  I don't want to mess with pointers and
     ;; buffer lengths just to hardcode a path to the "which"
     ;; executable.
     `(("which" ,which)))
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
     `(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; There is no configure phase.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
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

(define-public viennarna
  (package
    (name "viennarna")
    (version "2.2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.tbi.univie.ac.at/RNA/"
                                  "download/sourcecode/2_2_x/ViennaRNA-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0mycxjqci462d1zmcdhhc47360np1xcrdf2f3yrhhzbn5blwiwwl"))))
    (build-system gnu-build-system)
    (arguments
     ;; Disable link-time optimization because this creates problems
     ;; when stripping.  Linking with the stripped static library
     ;; would fail when LTO is enabled.  See the discussion here:
     ;; https://github.com/s-will/LocARNA/issues/7
     `(#:configure-flags '("--disable-lto")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-search-path
           (lambda _
             ;; Work around test failure.
             (setenv "PERL5LIB"
                     (string-append (getcwd) "/tests:"
                                    (getenv "PERL5LIB")))
             #t)))))
    (inputs
     `(("perl" ,perl)
       ("python" ,python)))
    (native-inputs
     `(("swig" ,swig)))
    (home-page "http://www.tbi.univie.ac.at/RNA/index.html")
    (synopsis "Prediction and comparison of RNA secondary structures")
    (description
     "RNA secondary structure prediction through energy minimization is the
most used function in the package.  Three kinds of dynamic programming
algorithms for structure prediction are provided: the minimum free energy
algorithm of Zuker & Stiegler (1981) which yields a single optimal structure,
the partition function algorithm of McCaskill (1990) which calculates base
pair probabilities in the thermodynamic ensemble, and the suboptimal folding
algorithm of Wuchty et.al (1999) which generates all suboptimal structures
within a given energy range of the optimal energy.  For secondary structure
comparison, the package contains several measures of
distance (dissimilarities) using either string alignment or
tree-editing (Shapiro & Zhang 1990).  Finally, an algorithm to design
sequences with a predefined structure (inverse folding) is provided.")
    (license (nonfree:non-free "TODO" "license forbids commercial usage"))))

(define-public viennarna-2.2.8
  (package (inherit viennarna)
    (version "2.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.tbi.univie.ac.at/RNA/packages/source/ViennaRNA-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0b9h3rrndxjvj3r2wyixf095fghpspgiwx3acbd8hlv3lj6hpi1h"))))))

(define-public viennarna-1.8
  (package (inherit viennarna)
    (version "1.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.tbi.univie.ac.at/RNA/download/sourcecode/"
                    "1_8_x/ViennaRNA-" version ".tar.gz"))
              (sha256
               (base32
                "1ygcs399xl07igj15ynfg6cd9ifi1amy8n0p6bl6awgpx95xkqpl"))))
    (arguments
     `(#:tests? #f ; no tests
       #:configure-flags
       (list "--without-perl"
	     "CFLAGS=-std=gnu89")))
    (inputs '())
    (native-inputs '())))

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
       ("viennarna" ,viennarna-2.2.8)))
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
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((target (string-append (assoc-ref outputs "out")
                                            "/share/nofold")))
                 (copy-recursively "." target))
               #t)))))
      (inputs
       `(("python" ,python-2)
         ("locarna" ,locarna)
         ("infernal" ,infernal-1.0)
         ("r" ,r)
         ("r-fastcluster" ,r-fastcluster)))
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
       ("boost" ,boost)
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

;; This package looks terrible because it doesn't have a build system
;; and depends on many outdated packages.
(define-public medicc
  (let ((commit "e440c3a2a4751b4c33fd2709844b751d1b545ea1")
        (revision "1"))
    (package
      (name "medicc")
      (version (string-append "0.0.0-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://bitbucket.org/rfs/medicc.git")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "0pky33bij6pi7z6473r8i8116vyyyv10wra5ki6kk1m6gpar9lcb"))))
      (build-system python-build-system)
      (arguments
       `(#:modules ((srfi srfi-26)
                    (guix build python-build-system)
                    (guix build utils))
         #:python ,python-2
         #:tests? #f ; there are no tests
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'prepare-compilation
             (lambda* (#:key inputs #:allow-other-keys)
               (copy-recursively (assoc-ref inputs "fstframework-sources")
                                 "lib/fstframework")
               ;; The "fstfar" library is in a subdirectory of the
               ;; openfst output.
               (setenv "LIBRARY_PATH"
                       (string-append (assoc-ref inputs "openfst")
                                      "/lib/fst:"
                                      (getenv "LIBRARY_PATH")))
               #t))
           (replace 'build
             (lambda _
               (with-directory-excursion "lib/fstframework/cExtensions"
                 (invoke "make"))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out     (assoc-ref outputs "out"))
                      (share   (string-append out "/share/medicc"))
                      (bin     (string-append out "/bin"))
                      (scripts '("cnv_find_loh_events.py"
                                 "cnv_find_precursor.py"
                                 "create_cnv_fst.py"
                                 "get_sequence_from_tree.py"
                                 "medicc_phase.py"
                                 "medicc.py"
                                 "medicc_segment.py"
                                 "medicc_snpphase.py")))
                 ;; Ensure that the non-standard directories are part
                 ;; of PYTHONPATH so that the "wrap" phase can do its
                 ;; work properly.
                 (setenv "PYTHONPATH"
                         (string-append share ":"
                                        share "/lib:"
                                        share "/lib/fstframework:"
                                        (or (getenv "PYTHONPATH") "")))
                 (mkdir-p share)
                 (mkdir-p bin)
                 (copy-recursively "." share)
                 (for-each (lambda (prog)
                             ;; Not all scripts are actually executable.
                             (chmod prog #o755)
                             (install-file prog bin)
                             (delete-file (string-append share "/" prog)))
                           scripts))
               #t)))))
      (inputs
       `(("python2-biopython" ,python2-biopython-1.62)
         ("python2-numpy" ,python2-numpy)
         ("python2-scipy" ,python2-scipy)))
      (native-inputs
       `(("fstframework-sources"
          ,(let ((commit "c371e4481183630229c99c24b747ce3d6b2b265f")
                 (revision "1"))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://bitbucket.org/rfs/fstframework.git")
                     (commit commit)))
               (file-name (string-append "fstframework-"
                                         "0.0.0-" revision "."
                                         (string-take commit 7)))
               (sha256
                (base32
                 "0s1483vz14fz1b7l8cs0hll70wn55dpfmdswmy7fqfq6w20q4lwl")))))))
      ;; FIXME: it would be better to patch the sources such that the
      ;; external executables are referenced by their full path.
      (propagated-inputs
       `(("phylip" ,phylip)
         ("openfst" ,openfst)
         ("weblogo" ,weblogo-3.3)))
      (synopsis "Minimum event distance for intra-tumour copy number comparisons")
      (description
       "MEDICC stands for \"Minimum Event Distance for Intra-tumour
Copy number Comparisons\".  No idea what this means.")
      (home-page "https://bitbucket.org/rfs/medicc")
      (license nonfree:undeclared))))

(define-public cofold
  (package
    (inherit viennarna-1.8)
    (name "cofold")
    (version "1.0")
    (source (origin
              (method url-fetch)
              ;; XXX uggh there's no versioning for this tarball,
              ;; should migrate it to git-based fetching asap.
              (uri (string-append "http://www.e-rna.org/cofold/CoFold.tar.gz"))
              (sha256
               (base32
                "1hr1hnm3nxj0y6yd94wxiqw10y653wyr6prl9i02a27bd6c27gbz"))))
    (arguments
     `(#:tests? #f ; there are no tests
       #:parallel-build? #f)) ; build fails otherwise
    (synopsis "Predict RNA secondary structure considering co-transcriptional folding")
    (description "CoFold is a thermodynamics-based RNA secondary
structure folding algorithm that takes co-transcriptional folding in
account.  This has been shown to significantly improve the
state-of-art in terms of prediction accuracy, especially for long
sequences greater than 1000 nt in length.")
    (home-page "http://www.e-rna.org/cofold/")
    (license (package-license viennarna-1.8))))

;; This is non-free because it contains ViennaRNA code, which is
;; released under a non-free license.
(define-public mafft-extensions
  (package (inherit mafft)
    (version (package-version mafft))
    (name "mafft-extensions")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://mafft.cbrc.jp/alignment/software/mafft-"
                                  version "-with-extensions-src.tgz"))
              (sha256
               (base32
                "0m8kdpw57j494j27m9c92virnq32xyfvfr306bh1wbxjz3l518kg"))))
    (arguments
     `(#:tests? #f ; no tests included
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "BINDIR=" out "/bin")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "extensions") #t))
         (delete 'configure))))
    (synopsis "Extensions for the MAFFT multiple sequence alignment package")
    (description
     "The extensions code includes code of the ViennaRNA package,
MXSCARNA and ProbConsRNA.")
    ;; FIXME: this is probably inaccurate.
    (license (package-license viennarna))))

(define-public r-rbowtie
  (package
    (name "r-rbowtie")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rbowtie" version))
       (sha256
        (base32
         "11sl03ljkq7hkm6ilw91bh4p2lz6iw35xn0f5fyh37i0j5nrmikh"))))
    (properties `((upstream-name . "Rbowtie")))
    (build-system r-build-system)
    (home-page "http://bioconductor.org/packages/Rbowtie")
    (synopsis "R bowtie wrapper")
    (description
     "This package provides an R wrapper around the popular bowtie
short read aligner and around SpliceMap, a de novo splice junction
discovery and alignment tool.  The package is used by the QuasR
bioconductor package.  It is recommended to use the QuasR package
instead of using Rbowtie directly.")
    (license nonfree:artistic1.0)))

(define-public r-quasr
  (package
    (name "r-quasr")
    (version "1.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "QuasR" version))
       (sha256
        (base32
         "1330vlvsfypr0pl7arawks4d80yjk2afn637ivcnbhrchkmicfdv"))))
    (properties `((upstream-name . "QuasR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biobase" ,r-biobase)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biocinstaller" ,r-biocinstaller)
       ("r-biocparallel" ,r-biocparallel)
       ("r-biostrings" ,r-biostrings)
       ("r-bsgenome" ,r-bsgenome)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-genomicfiles" ,r-genomicfiles)
       ("r-genomicranges" ,r-genomicranges)
       ("r-iranges" ,r-iranges)
       ("r-rbowtie" ,r-rbowtie)
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
    ;; Although this is free software, it depends on rbowtie, which is
    ;; nonfree.
    (license license:gpl2)))

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
       (snippet
        '(begin
           ;; Delete bundled stuff
           (delete-file-recursively "external")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags
       (list "ZLIB=-lz"
             (string-append "BAMTOOLSSRC="
                            (assoc-ref %build-inputs "bamtools")
                            "/lib/bamtools/libbamtools.a")
             (string-append "BAMTOOLSDIR="
                            (assoc-ref %build-inputs "bamtools")
                            "/include/bamtools"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'chdir
           (lambda _ (chdir "tools") #t))
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
                "#include <bamtools/api/BamReader.h>"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
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
                  "bamfastq")))
             #t)))))
    (inputs
     `(("zlib" ,zlib)
       ("boost" ,boost)
       ("bamtools" ,bamtools-2.0)
       ("samtools" ,samtools-0)))
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

(define-public r-scde
  (package
    (name "r-scde")
    (version "1.99.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hms-dbmi/scde/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ykvmnrjqiq5p9hfr568ndfx46fqdsi5jjp0i6s098y5i27455xg"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-against-armadillo
           (lambda _
             (substitute* "src/Makevars"
               (("PKG_LIBS =" prefix)
                (string-append prefix "-larmadillo"))))))))
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-mgcv" ,r-mgcv)
       ("r-rook" ,r-rook)
       ("r-rjson" ,r-rjson)
       ("r-cairo" ,r-cairo)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-edger" ,r-edger)
       ("r-quantreg" ,r-quantreg)
       ("r-nnet" ,r-nnet)
       ("r-rmtstat" ,r-rmtstat)
       ("r-extremes" ,r-extremes)
       ("r-pcamethods" ,r-pcamethods)
       ("r-biocparallel" ,r-biocparallel)
       ("r-flexmix" ,r-flexmix)))
    (home-page "http://hms-dbmi.github.io/scde/")
    (synopsis "R package for analyzing single-cell RNA-seq data")
    (description "The SCDE package implements a set of statistical
methods for analyzing single-cell RNA-seq data.  SCDE fits individual
error models for single-cell RNA-seq measurements.  These models can
then be used for assessment of differential expression between groups
of cells, as well as other types of analysis.  The SCDE package also
contains the pagoda framework which applies pathway and gene set
overdispersion analysis to identify aspects of transcriptional
heterogeneity among single cells.")
    ;; TODO: it's not clear if this is true.  See
    ;; https://github.com/hms-dbmi/scde/issues/38
    (license license:gpl2)))

(define-public r-cner
  (package
    (name "r-cner")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CNEr" version))
       (sha256
        (base32
         "051d1npid0avwr80jrbhq9igd7rd9dqm697aa7ai43z9a7lj1nnd"))))
    (properties `((upstream-name . "CNEr")))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-annotate" ,r-annotate)
       ("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-genomeinfodb" ,r-genomeinfodb)
       ("r-genomicalignments" ,r-genomicalignments)
       ("r-genomicranges" ,r-genomicranges)
       ("r-ggplot2" ,r-ggplot2)
       ("r-go-db" ,r-go-db)
       ("r-iranges" ,r-iranges)
       ("r-keggrest" ,r-keggrest)
       ("r-powerlaw" ,r-powerlaw)
       ("r-r-utils" ,r-r-utils)
       ("r-readr" ,r-readr)
       ("r-reshape2" ,r-reshape2)
       ("r-rsqlite" ,r-rsqlite)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-xvector" ,r-xvector)))
    (home-page "https://github.com/ge11232002/CNEr")
    (synopsis "CNE Detection and Visualization")
    (description
     "This package provides tools for large-scale identification and
advanced visualization of sets of conserved noncoding elements.")
    ;; src/ucsc is non-free (derived from Kent Utils)
    (license (list license:gpl2
                   (nonfree:non-free "Academic use only.")))))

(define-public blat
  (package
    (name "blat")
    (version "35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://users.soe.ucsc.edu/~kent/src/blatSrc"
                           version ".zip"))
       (sha256
        (base32
         "081nwnd97p2ry4rjnnia6816cssm682hlm7hzqhlnjpc2kqvrn86"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There is no test target
       #:make-flags (list "MACHTYPE=i386"
                          "BINDIR=/tmp/bin")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _ (mkdir-p "/tmp/bin") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (copy-recursively "/tmp/bin" bin))
             #t)))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("libpng" ,libpng)))
    (home-page "http://genome.ucsc.edu")
    (synopsis "Pairwise sequence alignment algorithm")
    (description "BLAT is a pairwise sequence alignment algorithm
that.  It was designed primarily to decrease the time needed to align
millions of mouse genomic reads and expressed sequence tags against
the human genome sequence.")
    (license (nonfree:non-free "Personal and academic use only."))))

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
    (arguments '(#:tests? #f))
    (inputs
     `(("perl" ,perl)))
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
       `(#:tests? #f          ; no "check" target
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           ;; There is no "install" target.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out")
                                         "/bin")))
                 (mkdir-p bin)
                 (install-file "bin/MUSIC" bin))
               #t)))))
      (home-page "http://music.gersteinlab.org")
      (synopsis "Multiscale enrichment calling for ChIP-Seq datasets")
      (description
       "MUSIC is an algorithm for identification of enriched regions
at multiple scales in the read depth signals from ChIP-Seq
experiments.")
      ;; https://github.com/gersteinlab/MUSIC/issues/6
      (license nonfree:undeclared))))

(define-public cufflinks
  (package
    (name "cufflinks")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://cole-trapnell-lab.github.io/"
                                  "cufflinks/assets/downloads/cufflinks-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1bnm10p8m7zq4qiipjhjqb24csiqdm1pwc8c795z253r2xk6ncg8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
        ;; The includes for "eigen" are located in a subdirectory.
        (string-append "EIGEN_CPPFLAGS="
                       "-I" (assoc-ref %build-inputs "eigen")
                       "/include/eigen3/")
        ;; Cufflinks must be linked with various boost libraries.
        (string-append "LDFLAGS="
                       (string-join '("-lboost_system"
                                      "-lboost_serialization"
                                      "-lboost_thread"))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-search-for-bam
           (lambda _
             (substitute* '("ax_bam.m4"
                            "configure"
                            "src/hits.h")
               (("<bam/sam\\.h>") "<samtools/sam.h>")
               (("<bam/bam\\.h>") "<samtools/bam.h>")
               (("<bam/version\\.hpp>") "<samtools/version.h>"))
             #t)))
       #:configure-flags
       (list (string-append "--with-bam="
                            (assoc-ref %build-inputs "samtools")))))
    (inputs
     `(("eigen" ,eigen)
       ("samtools" ,samtools-0.1)
       ("htslib" ,htslib)
       ("boost" ,boost-1.55)
       ("python" ,python-2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gcc" ,gcc-4.9)))
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
    (license (list license:boost1.0
                   (nonfree:non-free "Not to be used for benchmarks")))))

(define-public python2-rnaseqlib
  (let ((commit "2e8fb07190175c54101d16364072d7372c07815b")
        (revision "1"))
    (package
      (name "python2-rnaseqlib")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yarden/rnaseqlib.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1i7cj7c3k7hfx647dlpddnaf24pvia5k35f2fchmxy6bgppy0xky"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'fix-permissions
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files (assoc-ref outputs "out") ".*\\.gz$"))
               #t)))))
      (propagated-inputs
       `(("python2-cutadapt" ,python2-cutadapt)
         ("miso" ,miso)
         ("python2-scipy" ,python2-scipy) ; FIXME: used by miso
         ("python2-matplotlib" ,python2-matplotlib)
         ("python2-numpy" ,python2-numpy)
         ("python2-pandas" ,python2-pandas)
         ("python2-pybedtools" ,python2-pybedtools)
         ("python2-pysam" ,python2-pysam)))
      (home-page "http://yarden.github.com/rnaseqlib")
      (synopsis "RNA-Seq pipeline")
      (description "RNAseqlib is a simple pipeline for RNA-Seq
analysis.  It supports mRNA-Seq, Ribo-Seq, and CLIP-Seq analyses.")
      (license nonfree:undeclared))))

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

;; Missing license; see https://github.com/velocyto-team/velocyto.R/issues/24
(define-public r-velocyto
  (let ((commit "ca19f83c947b2e5f5e8484311b30f3c759fa069a")
        (revision "1"))
    (package
      (name "r-velocyto")
      (version (string-append "0.5-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/velocyto-team/velocyto.R.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1faahnf6msql8bfwq7346ibh3lqmx2qlqnykk06n6xsy1xyj6pf8"))))
      (build-system r-build-system)
      (inputs
       `(("boost" ,boost)))
      (propagated-inputs
       `(("r-mass" ,r-mass)
         ("r-mgcv" ,r-mgcv)
         ("r-pcamethods" ,r-pcamethods)
         ("r-rcpp" ,r-rcpp)
         ("r-rcpparmadillo" ,r-rcpparmadillo)
         ;; Suggested packages
         ("r-rtsne" ,r-rtsne)
         ("r-cluster" ,r-cluster)
         ("r-abind" ,r-abind)
         ("r-h5" ,r-h5)
         ("r-biocgenerics" ,r-biocgenerics)
         ("r-genomicalignments" ,r-genomicalignments)
         ("r-rsamtools" ,r-rsamtools)
         ("r-edger" ,r-edger)
         ("r-igraph" ,r-igraph)))
      (home-page "http://velocyto.org")
      (synopsis "RNA velocity estimation in R")
      (description
       "This package provides basic routines for estimation of
gene-specific transcriptional derivatives and visualization of the
resulting velocity patterns.")
      (license nonfree:undeclared))))

;; Bismark currently includes nonfree JavaScript
(define-public bismark-nonfree
  (package
    (name "bismark-nonfree")
    (version "0.19.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FelixKrueger/Bismark.git")
             (commit version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0yb5l36slwg02fp4b1jdlplgljcsxgqfzvzihzdnphd87dghcc84"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share   (string-append out "/share/bismark"))
                    (docdir  (string-append out "/share/doc/bismark"))
                    (docs    '("Docs/Bismark_User_Guide.html"))
                    (scripts '("bismark"
                               "bismark_genome_preparation"
                               "bismark_methylation_extractor"
                               "bismark2bedGraph"
                               "bismark2report"
                               "coverage2cytosine"
                               "deduplicate_bismark"
                               "filter_non_conversion"
                               "bam2nuc"
                               "bismark2summary")))
               (substitute* "bismark2report"
                 (("\\$RealBin/bismark_sitrep")
                  (string-append share "/bismark_sitrep")))
               (mkdir-p share)
               (mkdir-p docdir)
               (mkdir-p bin)
               (for-each (lambda (file) (install-file file bin))
                         scripts)
               (for-each (lambda (file) (install-file file docdir))
                         docs)
               (copy-recursively "Docs/Images" (string-append docdir "/Images"))
               (copy-recursively "bismark_sitrep"
                                 (string-append share "/bismark_sitrep"))

               ;; Fix references to gunzip
               (substitute* (map (lambda (file)
                                   (string-append bin "/" file))
                                 scripts)
                 (("\"gunzip -c")
                  (string-append "\"" (assoc-ref inputs "gzip")
                                 "/bin/gunzip -c")))
               #t))))))
    (inputs
     `(("gzip" ,gzip)))
    (home-page "http://www.bioinformatics.babraham.ac.uk/projects/bismark/")
    (synopsis "Map bisulfite treated sequence reads and analyze methylation")
    (description "Bismark is a program to map bisulfite treated sequencing
reads to a genome of interest and perform methylation calls in a single step.
The output can be easily imported into a genome viewer, such as SeqMonk, and
enables a researcher to analyse the methylation levels of their samples
straight away.  Its main features are:

@itemize
@item Bisulfite mapping and methylation calling in one single step
@item Supports single-end and paired-end read alignments
@item Supports ungapped and gapped alignments
@item Alignment seed length, number of mismatches etc are adjustable
@item Output discriminates between cytosine methylation in CpG, CHG
  and CHH context
@end itemize\n")
    (license license:gpl3+)))

(define-public sylamer
  (package
    (name "sylamer")
    (version "12-342")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://wwwdev.ebi.ac.uk/enright-dev/sylamer/"
                                  "sylamer-" version ".tgz"))
              (sha256
               (base32
                "0g82x2drm24nfzlvbahwy6vsbnqyy2qbrb7nsm82ja8zaim3mc7f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:make-flags
       (list (string-append "GSLPREFIX=" (assoc-ref %build-inputs "gsl")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("cp sylamer \\$\\(HOME\\)/local/bin")
                (string-append "install -D -t " (assoc-ref outputs "out")
                               "/bin sylamer")))
             #t)))))
    (inputs
     `(("gsl" ,gsl)
       ("zlib" ,zlib)))
    (home-page "https://www.ebi.ac.uk/research/enright/software/sylamer")
    (synopsis "Asses microRNA binding and siRNA off-target effects")
    (description "Sylamer is a system for finding significantly over or
under-represented words in sequences according to a sorted gene list.
Typically it is used to find significant enrichment or depletion of microRNA
or siRNA seed sequences from microarray expression data.  Sylamer is extremely
fast and can be applied to genome-wide datasets with ease.  Results are
plotted in terms of a significance landscape plot.  These plots show
significance profiles for each word studied across the sorted genelist.")
    ;; Contacted the authors; waiting for a reply.
    (license nonfree:undeclared)))

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
     `(#:tests? #f                      ; no test target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Ensure that we can override the target directory
               (substitute* "perl-scripts/configure_rsat.pl"
                 (("\\$rsat_parent_path = .*;")
                  (string-append "$rsat_parent_path = \""
                                 out "\";")))
               ;; Override the target directory
               (setenv "RSAT" out)
               ;; Target directory must exist
               (mkdir-p "bin")
               (invoke "perl" "perl-scripts/configure_rsat.pl"))
             #t))
         (replace 'build
           (lambda _
             ;; FIXME: this first step is pretty useless, because it
             ;; creates directories not in the install location but in
             ;; the build directory.
             (invoke "make" "-f" "makefiles/init_rsat.mk" "init")
             (invoke "make" "-f" "makefiles/init_rsat.mk" "compile_all")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "bin" (string-append out "/bin"))
               #t))))))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "http://rsat.eead.csic.es/plants/")
    (synopsis "Regulatory sequence analysis tools")
    (description "This package provides a subset of the Regulatory
Sequence Analysis Tools (RSAT).")
    (license (nonfree:non-free
              "The stand-alone version is freely available for academic
users, with some restrictions on utilization (non-commercial,
non-military and non-redistribution)."))))

(define-public r-minet
  (package
    (name "r-minet")
    (version "3.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "minet" version))
       (sha256
        (base32
         "14vyl2f27ydffz7y7yqg5rja5rv2zrzadfm23wp2633qisw56rpr"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-infotheo" ,r-infotheo)))
    (home-page "http://minet.meyerp.com")
    (synopsis "Mutual information networks")
    (description
     "This package implements various algorithms for inferring mutual
information networks (such as gene networks) from data (using mutual
information). In particular, the package implements MRNET.")
    (license (nonfree:non-free "CC-BY-NC-SA"))))

(define-public python2-mace
  (package
    (name "python2-mace")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/chipexo/MACE-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0v9qcxqi35hzcb28v29cr6r58dn7197pwzxqxkkqfhsdgpiia2ih"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2               ; only Python 2.7 is supported
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-setuptools
           (lambda _
             (substitute* "setup.py"
               (("from distribute_setup.*") "")
               (("use_setuptools\\(\\)") ""))
             #t)))))
    ;; XXX: This bundles sources of other packages, such as pysam, parts of
    ;; kentutils, bunzip, pymix, bx, tabix, exomodule, etc...
    (inputs
     `(("gsl" ,gsl)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("python-numpy" ,python2-numpy)))
    (native-inputs
     `(("python-nose" ,python2-nose)
       ("python-cython" ,python2-cython)))
    (home-page "http://chipexo.sourceforge.net")
    (synopsis "Model-based analysis of ChIP-exo data")
    (description "ChIP-exo allows for precise mapping of protein-DNA
interactions.  It uses λ phage exonuclease to digest the 5’ end of
protein-unbound DNA fragments and thereby creates a homogenous 5’ end at a
fixed distance from the protein binding location.  After sequencing and
aligning reads to the reference genome, the 5’ ends of reads align primarily
at two genomic locations corresponding to two borders of protein binding site.
MACE is a bioinformatics tool dedicated to analyze ChIP-exo data.")
    ;; The license is rather unclear.  Some source headers say "MIT license",
    ;; others say "free for all use", others say "GPL version 1.3" (?), yet
    ;; others say "Artistic license".
    (license license:gpl2+)))

;; Although this program is released under the GPL it depends on
;; ViennaRNA and bowtie1, which are non-free software.
(define-public mirdeep2
  (package
    (name "mirdeep2")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                   (url "https://github.com/rajewsky-lab/mirdeep2.git")
                   (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0srxxymxmpfb656g6r26zhplx9fl6wxrzzdl6wr7r06nnmx6sjd2"))))
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
                 (const #t)))  ;; simply touch the file
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
