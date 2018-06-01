;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; However, note that this module provides packages for "non-free" software,
;;; which denies users the ability to study and modify it.  These packages
;;; are detrimental to user freedom and to proper scientific review and
;;; experimentation.  As such, we kindly invite you not to share it.
;;;
;;; Copyright © 2018 Inria

(define-module (non-free cuda)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (ice-9 match))

(define (make-cuda version origin)
  (package
    (name "cuda-toolkit")
    (version version)
    (source origin)
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;196 MiB
    (arguments
     `(#:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match))

       #:strip-binaries? #f                       ;no need

       ;; XXX: This would check DT_RUNPATH, but patchelf populate DT_RPATH,
       ;; not DT_RUNPATH.
       #:validate-runpath? #f

       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((source (assoc-ref inputs "source")))
                        (invoke "sh" source "--keep" "--noexec")
                        (chdir "pkg/run_files")
                        (match (find-files "." "^cuda-linux64-rel.*\\.run$")
                          ((run)
                           (invoke "sh" run "--keep" "--noexec")))
                        (chdir "pkg"))))
                  (delete 'configure)
                  (delete 'check)
                  (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (define out
                        (assoc-ref outputs "out"))
                      (define libc
                        (assoc-ref inputs "libc"))
                      (define gcc-lib
                        (assoc-ref inputs "gcc:lib"))
                      (define ld.so
                        (string-append libc ,(glibc-dynamic-linker)))
                      (define rpath
                        (string-join (list "$ORIGIN"
                                           (string-append out "/lib")
                                           (string-append out "/nvvm/lib64")
                                           (string-append libc "/lib")
                                           (string-append gcc-lib "/lib"))
                                     ":"))

                      (define (patch-elf file)
                        (unless (string-contains file ".so")
                          (format #t "Setting interpreter on '~a'...~%" file)
                          (invoke "patchelf" "--set-interpreter" ld.so
                                  file))
                        (format #t "Setting RPATH on '~a'...~%" file)
                        (invoke "patchelf" "--set-rpath" rpath
                                "--force-rpath" file))

                      (for-each (lambda (file)
                                  (when (elf-file? file)
                                    (patch-elf file)))
                                (find-files "."
                                            (lambda (file stat)
                                              (eq? 'regular
                                                   (stat:type stat)))))
                      #t))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out   (assoc-ref outputs "out"))
                             (lib   (string-append out "/lib"))
                             (lib64 (string-append out "/lib64")))
                        (mkdir-p out)
                        (setenv "PERL5LIB" (getcwd)) ;for InstallUtils.pm
                        (invoke "perl" "install-linux.pl"
                                (string-append "--prefix=" out))
                        (rename-file lib64 lib)
                        #t)))
                  (add-after 'install 'move-documentation
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out    (assoc-ref outputs "out"))
                             (doc    (assoc-ref outputs "doc"))
                             (docdir (string-append doc "/share/doc/cuda")))
                        (mkdir-p (dirname docdir))
                        (rename-file (string-append out "/doc") docdir)
                        #t))))))
    (native-inputs
     `(("patchelf" ,patchelf)
       ("perl" ,perl)
       ("python" ,python-2)))
    (inputs
     `(("gcc:lib" ,gcc "lib")))
    (synopsis
     "Compiler for the CUDA language and associated run-time support")
    (description
     "This package provides the CUDA compiler and the CUDA run-time support
libraries for NVIDIA GPUs, all of which are proprietary.")
    (home-page "https://developer.nvidia.com/cuda-toolkit")
    (license #f)
    (supported-systems '("x86_64-linux"))))

(define-syntax-rule (cuda-source url hash)
  (origin
    (uri url)
    (sha256 (base32 hash))
    (method url-fetch)))

(define-public cuda-8.0
  (make-cuda "8.0.61"
             (cuda-source
              "https://developer.nvidia.com/compute/cuda/8.0/Prod2/local_installers/cuda_8.0.61_375.26_linux-run"
              "1i4xrsqbad283qffvysn88w2pmxzxbbby41lw0j1113z771akv4w")))

(define-public cuda
  ;; Default version.
  cuda-8.0)

(define-public no-float128
  ;; FIXME: We cannot simply add it to 'propagated-inputs' of cuda-toolkit
  ;; because then it would come after glibc in CPLUS_INCLUDE_PATH.
  (package
    (name "no-float128")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))

                   (let* ((header "/include/bits/floatn.h")
                          (out    (assoc-ref %outputs "out"))
                          (target (string-append out (dirname header)))
                          (libc   (assoc-ref %build-inputs "libc")))
                     (mkdir-p target)
                     (install-file (string-append libc header) target)
                     (substitute* (string-append target "/" (basename header))
                       (("#([[:blank:]]*)define __HAVE_FLOAT128[[:blank:]]+1"
                         _ space)
                        (string-append "#" space
                                       "define __HAVE_FLOAT128 0")))
                     #t))))
    (inputs `(("libc" ,glibc)))
    (synopsis "@file{<bits/floatn.h>} header that disables float128 support")
    (description
     "This package provides a @file{<bits/floatn.h>} header to override that
of glibc and disable float128 support.  This is required allow the use of
@command{nvcc} with glibc 2.26+.  Otherwise, @command{nvcc} fails like this:

@example
/gnu/store/…-glibc-2.26.105-g0890d5379c/include/bits/floatn.h(61): error: invalid argument to attribute \"__mode__\"

/gnu/store/…-glibc-2.26.105-g0890d5379c/include/bits/floatn.h(73): error: identifier \"__float128\" is undefined
@end example

See also
@url{https://devtalk.nvidia.com/default/topic/1023776/cuda-programming-and-performance/-request-add-nvcc-compatibility-with-glibc-2-26/1}.")
    (home-page "https://hpc.guixsd.org")
    (license license:gpl3+)))
