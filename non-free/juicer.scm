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
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl))

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

(define-public juicer
  (let ((commit "43cc35ff546d33839b89ca3949c2e1f848077eb1")
        (revision "1"))
    (package
      (name "juicer")
      (version (git-version "1.6.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/aidenlab/juicer.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0gcq9klgwwyq4y0pr0w6a2vfsf8v5a16zqpgqfks5yh6dyrj2ha9"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; there are none!
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "UGER/scripts/juicer.sh"
                 (("^export TMPDIR.*") "") ; why would you even do that?
                 (("java -")
                  (string-append (which "java") " -"))
                 (("#!/bin/bash")
                  (string-append "#!" (which "bash")))
                 ;; This assumes the use of environment modules.
                 ;; Disable this.
                 (("^usePath=/broad/software/scripts/useuse")
                  "usePath=echo")
                 (("source \\$usePath") "")
                 (("\\$load_cluster") "")
                 (("^load_bwa=.*") "load_bwa=echo\n")
                 (("^load_java=.*") "load_java=echo\n")
                 (("^load_cluster=.*") "load_cluster=echo\n")
                 (("^load_coreutils=.*") "load_coreutils=echo\n")
                 (("^juiceDir=\"/broad/aidenlab\"") "juiceDir=\"/tmp\"")
                 (("\\$\\{juiceDir\\}/scripts")
                  (string-append (assoc-ref outputs "out")
                                 "/share/juicer/UGER/scripts")))
               (substitute* "UGER/scripts/juicer_tools"
                 ;; Use this particular variant of Java.
                 (("^java ")
                  (string-append (which "java") " "))
                 ;; Don't try to find juicer_tools in the same
                 ;; directory as the script.
                 (("`dirname \\$0`/juicer_tools.jar")
                  (string-append (assoc-ref inputs "java-juicebox")
                                 "/share/java/juicer_tools.jar")))))
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (share (string-append out "/share/juicer")))
                 (copy-recursively "." share)
                 (mkdir-p bin)
                 ;; Simple script to launch the selected pipeline
                 ;; script.  Default to UGER (grid engine) scripts.
                 (with-output-to-file (string-append bin "/juicer")
                   (lambda ()
                     (format #t "#!~a~%dir=\"$1\"; test -z \"$dir\" && dir=UGER; shift;~%~a \"~a/$dir/scripts/juicer.sh\" $@~%"
                             (which "bash")
                             (which "bash")
                             share)))
                 (chmod (string-append bin "/juicer") #o555)
                 #t))))))
      (inputs
       `(("bash" ,bash)
         ("java-juicebox" ,java-juicebox)
         ("java" ,icedtea)
         ("perl" ,perl)))
      (propagated-inputs
       `(("bwa" ,bwa)
         ("coreutils" ,coreutils)
         ("gawk" ,gawk)))
      (home-page "https://github.com/aidenlab/Juicebox")
      (synopsis "Visualization and analysis software for Hi-C data")
      (description "Juicebox is visualization software for Hi-C data.
Juicebox can now be used to visualize and interactively (re)assemble
genomes.")
      (license license:expat))))
