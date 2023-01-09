(define-module (akira emacs-xyz-latest)
  #:use-module (guix packages)
  #:use-module (guix git)
  #:use-module (guix git-download)
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

; I should really make -defs more minimal and construct everything I want to export then dynamically call (export )
(include "emacs-xyz-latest-defs.scm")

(define-public with-latest-emacs-packages
  (package-input-rewriting %emacs-package-latest-replacements))
