(define-module (akira emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (emacs-latest utils)
  #:use-module (emacs-latest emacs-xyz))

;; TODO upstream these
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

(define-public emacs-consult-flyspell
  (package
    (name "emacs-consult-flyspell")
    (version "HEAD")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/OlMon/consult-flyspell.git")
                    (commit "d587961ef0d5f9992cedef3b35b87b11d610a375")))
              (sha256
               (base32
                "0sz64k5fb0jpm2cmynfnnsslb1ka86zq1fdvc5ipbysiqnplfjpb"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-consult))
    (home-page "https://gitlab.com/OlMon/consult-flyspell")
    (synopsis "A package to incorporate flyspell into consult. ")
    (description
     "A package to incorporate flyspell into consult.
  This allows to choose a misspelled word, jump to it and optionally
  apply a function to it.")
    (license license:gpl3+)))

(define-public emacs-aas
  (package
    (name "emacs-aas")
    (version "HEAD")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ymarco/auto-activating-snippets.git")
                    (commit "e92b5cffa4e87c221c24f3e72ae33959e1ec2b68")))
              (sha256
               (base32
                "1nl7wm4l30hjcbqrvdci66aa6ax32ih46n58q3imc46z8c6rhqxh"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ymarco/auto-activating-snippets")
    (synopsis " Snippets for Emacs that expand as you type")
    (description
     "This package implements an engine for auto-expanding snippets. It is done
by tracking your inputted chars along a tree until you complete a registered key
sequence. Its like running a long prefix command, but the keys you type are not
‘consumed’ and appear in the buffer until you complete the whole command - and
then the snippet is triggered!")
    (license license:gpl3+)))

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

(define-public my-emacs-org
  (package
    (inherit (emacs-xyz-latest emacs-org))
    (source
     (origin
       (inherit (package-source (emacs-xyz-latest emacs-org)))
       (patches (list (local-file "emacs-org.patch")))))))


(define-public my-emacs-flyspell-correct
  (package
    (inherit (emacs-xyz-latest emacs-flyspell-correct))
    (arguments `(#:include '("^flyspell-correct.el$")))
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       (emacs-xyz-latest emacs-flyspell-correct))
                         (delete "emacs-helm")
                         (delete "emacs-ivy")
                         (delete "emacs-popup")))))

;(define-public mu-latest
;  (package-commit mu
;                  "c23dad70586bbb54891c506629f2ce2ed8e463d2"
;                  "0hy6vxsj18wdghgc7h5v3asw23j5cnr0vamk7x8idg74n75sg6nm"))

(define-public my-emacs-replacements
  (package-input-rewriting
   `((,(emacs-xyz-latest emacs-jupyter) . ,my-emacs-jupyter)
     (,(emacs-xyz-latest emacs-flyspell-correct) . ,my-emacs-flyspell-correct)
     (,(emacs-xyz-latest emacs-org) . ,my-emacs-org))))

(define-public %all-my-emacs-packages
  (map my-emacs-replacements
    (map emacs-xyz-latest
      (list
       emacs-doom-themes
       emacs-doom-modeline
       emacs-general
       emacs-which-key
       emacs-undo-tree
       emacs-evil
       emacs-evil-collection
       emacs-evil-matchit
       emacs-evil-surround
       emacs-evil-tex
       emacs-evil-org
       emacs-avy
       emacs-ace-link
       emacs-ace-window
       emacs-vertico
       emacs-orderless
       emacs-consult
       emacs-consult-flyspell
       emacs-flyspell-correct
       emacs-consult-eglot
       emacs-marginalia
       emacs-embark
       emacs-corfu
       emacs-cape
       emacs-tempel
       emacs-kind-icon
       emacs-rainbow-delimiters
       emacs-smartparens
       emacs-aas
       emacs-org
       emacs-org-roam
       emacs-org-contrib
       emacs-org-pdftools
       emacs-htmlize
       emacs-citeproc-el
       emacs-citar
       emacs-citar-org-roam
       emacs-all-the-icons-completion
       emacs-direnv
       emacs-buffer-env
       emacs-pass
       emacs-vterm
       emacs-pdf-tools
       emacs-magit
       emacs-auctex
       emacs-geiser-guile
       emacs-julia-mode
       emacs-haskell-mode
       emacs-markdown-mode
       emacs-json-mode
       emacs-gcmh
       emacs-jupyter
       mu
       ))))
