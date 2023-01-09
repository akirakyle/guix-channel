(define-module (akira emacs-xyz-latest-utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:use-module (guix read-print))

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
  (let ((new-sym (symbol-append sym '-latest)))
    (cons sym (cons new-sym
                    `(define-public ,new-sym
                       (package-commit ,sym ,@(latest-commit-and-hash pkg)))))))

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

(define (make-replacements-helper el)
  (let ((sym (car el))
        (new-sym (cadr el)))
    ``(cons ,,sym ,,new-sym)))

(define (make-replacements pkgs)
  `((define-public %emacs-package-latest-replacements
      (list ,@(map make-replacements-helper pkgs)))))

(define-public (latest-emacs-xyz file)
  (let* ((emacs-packages
          (module-map (lambda (sym var) (cons sym var))
                      (resolve-module '(gnu packages emacs-xyz))))
         (packages-latest ;(filter-map package-latest-handler emacs-packages))
         (filter-map package-latest-handler (take emacs-packages 10)))
         (defs (map cddr packages-latest))
         (replacements (make-replacements packages-latest)))
    (with-atomic-file-output file
      (lambda (port)
        (pretty-print-with-comments/splice port defs)
        (pretty-print-with-comments/splice port replacements)))))
