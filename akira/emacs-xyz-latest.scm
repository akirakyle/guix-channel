(define-module (akira emacs-xyz-latest)
  #:use-module (guix packages)
  #:use-module (guix git)
  #:use-module (guix git-download))
  ;#:use-module (gnu packages emacs-xyz))

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

(define (package-from-data sym commit checksum)
  (let* ((pkg (module-ref (resolve-module '(gnu packages emacs-xyz)) sym))
         ;;(pkg (eval sym (resolve-module '(gnu packages emacs-xyz))))
         (new-sym (symbol-append sym '-latest))
         (latest-pkg (package-commit pkg commit checksum)))
    (module-add! (current-module) new-sym (make-variable latest-pkg))
    (export new-sym) ;; this doesn't work because export is syntax not a function
    (cons new-sym (cons pkg latest-pkg))))

(define pkg-data (call-with-input-file "akira/emacs-xyz-latest-data.scm" read))
(define pkgs (map (lambda (x) (apply package-from-data x)) pkg-data))

(define-public %emacs-package-latest-replacements (map cdr pkgs))

(define-public with-latest-emacs-packages
  (package-input-rewriting %emacs-package-latest-replacements))
