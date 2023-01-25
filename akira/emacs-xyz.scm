(define-module (akira emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (emacs-latest emacs-xyz)
  #:use-module (emacs-latest utils))

;; TODO upstream this
(define-public emacs-org-pdftools
  (package
    (name "emacs-org-pdftools")
    (version "HEAD")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fuxialexander/org-pdftools.git")
                    (commit "967f48fb5038bba32915ee9da8dc4e8b10ba3376")))
              (sha256
               (base32
                "0f47ww8r00b7lb1msybnmnqdhm9i2vwz5lrz9m9bn6gbh97mzhn8"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-home
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list emacs-org emacs-pdf-tools emacs-org-noter))
    (home-page "https://github.com/fuxialexander/org-pdftools")
    (synopsis "Support for links to documents in pdfview mode")
    (description
     "Add support for org links from pdftools buffers with more precise location
control.  https://github.com/fuxialexander/org-pdftools/")
    (license license:gpl3+)))

(define-public mu-latest
  (package-commit mu
                  "c23dad70586bbb54891c506629f2ce2ed8e463d2"
                  "0hy6vxsj18wdghgc7h5v3asw23j5cnr0vamk7x8idg74n75sg6nm"))

(define-public my-emacs-jupyter
  (package
    (inherit (emacs-xyz-latest emacs-jupyter))
    (source
     (origin
       (inherit (package-source (emacs-xyz-latest emacs-jupyter)))
       ;; https://github.com/nnicandro/emacs-jupyter/issues/380
       ;; can be triggered with raise Exception in python block
       (patches (list (local-file "emacs-jupyter.patch")))))
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       (emacs-xyz-latest emacs-jupyter))
                         (delete "emacs-company")
                         (delete "emacs-markdown-mode")))))


(define-public %all-my-emacs-packages
  (map with-emacs-xyz-latest
    (append 
     (map emacs-xyz-latest
       (list
        emacs-vterm
        ;;emacs-pdf-tools
        emacs-org-roam
        emacs-citar-org-roam
        emacs-all-the-icons-completion
        emacs-magit
        emacs-geiser-guile
        ))
     (list
      my-emacs-jupyter
      emacs-org-pdftools
      mu-latest
      ))))
