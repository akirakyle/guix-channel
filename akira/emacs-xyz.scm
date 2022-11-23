(define-module (akira emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages emacs-xyz))

(define (package-commit pkg commit checksum)
  "Return a package variant using the given commit and sha256."
  (package
    (inherit pkg)
    (name (package-name pkg))
    (version (substring commit 0 7))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (git-checkout-url (git-reference->git-checkout
                                     (origin-uri (package-source pkg)))))
             (commit commit)))
       (sha256 (base32 checksum))
       (file-name (git-file-name name version))))))

;; TODO fix upstream package definition so this can be simply overriden
(define-public emacs-zmq-latest
  (package
    (name "emacs-zmq")
    (version "HEAD")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nnicandro/emacs-zmq")
             (commit "af5299d80715b1083a18145e9c84ef9563020676")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jn1jkkl0pg2psncrf0rx9csp95pg9wm1pcmy1cb3kbqla9x27p4"))))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #f ; there are no tests to run
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda _
             (invoke "make" "src/configure")
             (substitute* "src/configure"
               (("/bin/sh") (which "sh"))
               (("/usr/bin/file") (which "file")))
             (invoke "make")))
         (add-after 'install 'install-shared-object
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site-lisp (string-append out "/share/emacs/site-lisp"))
                    (libdir (string-append site-lisp "/zmq-"
                                           ,(package-version this-package))))
               (copy-file "emacs-zmq.so"
                          (string-append libdir "/emacs-zmq.so"))))))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list zeromq))
    (home-page "https://github.com/nnicandro/emacs-zmq")
    (synopsis "Emacs bindings to ØMQ")
    (description "This package provides Emacs bindings to ØMQ.")
    (license (list gpl2+     ;zmq.el
                   gpl3+)))) ;src/emacs-module.h

(define-public emacs-jupyter-latest
  (package
    (inherit emacs-jupyter)
    (name "emacs-jupyter")
    (version "HEAD")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nnicandro/emacs-jupyter")
             (commit "0a7055d7b12cf98723110415b08ee91869fa7d94")))
       (file-name (git-file-name name version))
       (patches
          (list (local-file "emacs-jupyter.patch")))
       (sha256
        (base32 "183313jlmfnbndczllkqm47y4495prw4ks2jav3pdwn5qqfmpznx"))))
    (propagated-inputs (modify-inputs (package-propagated-inputs emacs-jupyter)
                         (delete "emacs-company")
                         (delete "emacs-markdown-mode")))))


(define-public emacs-pdf-tools-latest
  (package-commit emacs-pdf-tools "d6980bc3273e1cf1a73feee6bb523d1568405685"
                  "1a0l76k183fmkd7wjw86lf0mwcvkvd0gsyyh3p49z56094srxjar"))

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
    (license gpl3+)))

(define-public emacs-org-roam-with-emacssql
  (package
    (inherit emacs-org-roam)
    (name "emacs-org-roam")
    (propagated-inputs (modify-inputs (package-propagated-inputs emacs-org-roam)
                                      (delete "emacs-emacsql-sqlite3")
                                      (prepend emacs-emacsql)))))

(define-public emacs-all-the-icons-latest
  (package-commit emacs-all-the-icons
                  "51bf77da1ebc3c199dfc11f54c0dce67559f5f40"
                  "1idzamhpfgcdiwap20s3cc258kawxa1k46c4s79xslfbdqy0abdy"))

(define-public emacs-all-the-icons-completion-latest
  (package-commit emacs-all-the-icons-completion
                  "4d8ae544ecf5414c7ddefcf15ca6c3de4f627ef5"
                  "1cp5i01ln4j71gng38d03p2mdrvjgfcm29k4qjn5gzq6g4713wic"))

(define-public emacs-vterm-latest
  (package-commit emacs-vterm "a940dd2ee8a82684860e320c0f6d5e15d31d916f"
                  "0r1iz92sn2ddi11arr9s8z7cdpjli7pn55yhaswvp4sdch7chb5r"))

(define %emacs-packages-replacements
  `((,emacs-pdf-tools . ,emacs-pdf-tools-latest)
    (,emacs-org-roam . ,emacs-org-roam-with-emacssql)
    (,emacs-all-the-icons . ,emacs-all-the-icons-latest)
    (,emacs-all-the-icons-completion . ,emacs-all-the-icons-completion-latest)
    (,emacs-zmq . ,emacs-zmq-latest)
    (,emacs-vterm . ,emacs-vterm-latest)))

(define-public with-akira-emacs-packages
  (package-input-rewriting %emacs-packages-replacements))

(define-public %all-my-emacs-packages
  (map with-akira-emacs-packages
       (list emacs-jupyter-latest
             emacs-vterm-latest
             emacs-org-roam-with-emacssql
             emacs-citar-org-roam
             emacs-all-the-icons-completion
             emacs-pdf-tools-latest
             emacs-org-pdftools
             emacs-magit
             mu
             )))
