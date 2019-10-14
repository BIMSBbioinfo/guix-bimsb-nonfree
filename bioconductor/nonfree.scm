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

(define-module (bioconductor nonfree)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix licenses-nonfree) #:prefix nonfree:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages statistics))

(define-public r-motifdb
  (package
    (name "r-motifdb")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "MotifDb" version))
       (sha256
        (base32
         "05rxcxdkdpg0qg5qlylf0in9qhi16gpl8c9sfm4j2z3rvfxl0g60"))))
    (properties `((upstream-name . "MotifDb")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocgenerics" ,r-biocgenerics)
       ("r-biostrings" ,r-biostrings)
       ("r-iranges" ,r-iranges)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-splitstackshape" ,r-splitstackshape)))
    (home-page "https://bioconductor.org/packages/MotifDb")
    (synopsis "Annotated collection of protein-DNA binding sequence motifs")
    (description
     "This package provides more than 9900 annotated position
frequency matrices from 14 public sources, for multiple organisms.")
    ;; It's complicated... the data from public sources are under
    ;; different licenses; some are not licensed at all.
    (license (nonfree:non-free
              "https://bioconductor.org/packages/release/bioc/licenses/MotifDb/LICENSE"))))

(define-public r-cner
  (package
    (name "r-cner")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "CNEr" version))
       (sha256
        (base32
         "1filzgsyx22ckdspmqc6knc5gxw87vx0djzss6l69f50av8az1n8"))))
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

(define-public r-rankprod
  (package
    (name "r-rankprod")
    (version "3.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "RankProd" version))
       (sha256
        (base32
         "0530izdfqishc6jjnj0ac5pcvsdh1z646imwy8b1s95vgnq5qg8q"))))
    (properties `((upstream-name . "RankProd")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gmp" ,r-gmp)
       ("r-rmpfr" ,r-rmpfr)))
    (home-page "https://bioconductor.org/packages/RankProd/")
    (synopsis "Identify differentially expressed genes")
    (description
     "This package implements a non-parametric method for identifying
differentially expressed (up- or down- regulated) genes based on the estimated
percentage of false predictions (pfp).  The method can combine data sets from
different origins (meta-analysis) to increase the power of the
identification.")
    (license (nonfree:non-free "Non-commercial"))))
