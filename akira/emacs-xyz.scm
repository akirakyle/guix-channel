(define-module (akira emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-zmq-latest
  (package
    (name "emacs-zmq")
    (version "HEAD")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nnicandro/emacs-zmq")
             (commit "38dc6c4119aee57666caf8f97c8a3d7f678823e0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j7szww8fi2pyvln1bppyq8nly0vkbncz63kzqhi1zx7dfz127ry"))))
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
                    (site-lisp (string-append out "/share/emacs/site-lisp")))
               (copy-file "emacs-zmq.so"
                          (string-join `(,site-lisp "/zmq-" "HEAD" "/emacs-zmq.so") ""))))))))
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
                         (delete "emacs-markdown-mode")
                         (replace "emacs-zmq" emacs-zmq-latest)))))

(define-public emacs-vterm-latest
  (package
    (inherit emacs-vterm)
    (name "emacs-vterm")
    (version "HEAD")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/akermu/emacs-libvterm")
             (commit "a940dd2ee8a82684860e320c0f6d5e15d31d916f")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r1iz92sn2ddi11arr9s8z7cdpjli7pn55yhaswvp4sdch7chb5r"))))))

(define-public mu-latest
  (package
    (inherit mu)
    (name "mu")
    (version "1.8.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/djcb/mu")
                    (commit "44d3cefbf7f6751bb64fb822f7c459f3c8be7475")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m342pr1bmgv62lhl8g98wibz6rqjnbis9fqb6ivwi0ns0crvyyj"))))))

(define-public emacs-org-roam-with-emacssql
  (package
    (inherit emacs-org-roam)
    (name "emacs-org-roam")
    (propagated-inputs (modify-inputs (package-propagated-inputs emacs-org-roam)
                                      (delete "emacs-emacsql-sqlite3")
                                      (prepend emacs-emacsql)))))

(define-public emacs-citar-org-roam-with-emacssql
  (package
    (inherit emacs-citar-org-roam)
    (name "emacs-citar-org-roam")
    (propagated-inputs (modify-inputs (package-propagated-inputs emacs-citar-org-roam)
                                      (delete "emacs-org-roam")
                                      (prepend emacs-org-roam-with-emacssql)))))
