(define-module (akira emacs-xyz-latest)
  #:use-module (srfi srfi-1)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:use-module (guix read-print))

(define (package-commit pkg commit hash)
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

(define (latest-commit-and-hash pkg)
  (unless (eq? git-fetch (origin-method (package-source pkg)))
    (raise-exception "package doesn't use git-fetch\n"))
  (define-values (checkout ref)
    (with-store store
      (latest-repository-commit
       store
       (git-checkout-url (git-reference->git-checkout
                          (origin-uri (package-source pkg)))))))
  (list
   ref
   (bytevector->nix-base32-string
    (file-hash* checkout #:recursive? #true #:select? (const #true)))))

(define (package-latest sym pkg)
  `(define-public ,(symbol-append sym '-latest)
     (package-commit ,sym ,@(latest-commit-and-hash pkg))))

(define (package-latest-handler el)
  (with-exception-handler
      (lambda (e)
        (format #t "error for package: ~A\nexception: ~A\n" (car el) e)
        #f)
    (lambda ()
      (let ((sym (car el))
            (pkg (variable-ref (cdr el))))
        (package-latest sym pkg)))
    #:unwind? #t))

(define emacs-packages
  (module-map (lambda (sym var) (cons sym var))
              (resolve-module '(gnu packages emacs-xyz))))

(define emacs-packages-latest-info
  (filter-map package-latest-handler (take emacs-packages 20)))

(define* (write-latest file)
  (with-atomic-file-output file
    (lambda (port)
      (pretty-print-with-comments/splice port emacs-packages-latest-info))))

(write-latest "akira-emacs-xyz-latest.scm")

;(define pkg-lst
;  (module-map (lambda (sym var) (list sym var))
;              (resolve-module '(gnu packages emacs-xyz))))
;
;(assoc 'emacs-vterm pkg-lst)
;
;(fold-packages )
;(fold-module-public-variables)
;(latest-commit-and-hash (git-checkout-url (git-reference->git-checkout (origin-uri (package-source (variable-ref (cdr (assoc 'emacs-vterm pkg-lst))))))))
; just do this automatically for everything in emacs-xyz using
; https://lists.gnu.org/archive/html/guile-user/2016-06/msg00045.html
; turn this channel into an equivalent of nix emacs-overlay
