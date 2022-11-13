(define-module (akira texlab)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary))

(define-public texlab
  (package
   (name "texlab-bin")
   (version "4.3.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/latex-lsp/texlab/releases/download/v"
           version
           "/texlab-aarch64-linux.tar.gz"))
     (sha256
      (base32
       "1fnhi5chfwg7hddj9m882vy1rw4k6m48y5pgrmla16r63zq5ya0p"))))
   (build-system binary-build-system)
   (arguments
    `(#:install-plan
      `(("texlab" "/bin/"))
      #:patchelf-plan
      `(("texlab" ("gcc:lib")))
      #:phases
      (modify-phases %standard-phases
                     (replace 'unpack
                              (lambda* (#:key inputs #:allow-other-keys)
                                (invoke "tar" "-xvzf" (assoc-ref inputs "source")))))))
   (inputs
    `(("gcc:lib" ,gcc "lib")))
   (native-inputs
    `(("gzip" ,gzip)))
   (synopsis "An implementation of the Language Server Protocol for LaTeX ")
   (description "A cross-platform implementation of the Language Server Protocol providing rich cross-editing support for the LaTeX typesetting system. The server may be used with any editor that implements the Language Server Protocol.")
   (home-page "https://texlab.netlify.app/")
   (license license:gpl3)))

