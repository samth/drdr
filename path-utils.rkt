#lang racket/base
(require racket/list
         racket/path
         racket/contract/base
         racket/file
         "notify.rkt")

(define current-temporary-directory
  (make-parameter #f))

(define (directory-list->directory-list* l)
  (sort (filter-not (compose 
                     (lambda (s)
                       (or (regexp-match #rx"^\\." s)
                           (string=? "compiled" s)
                           (link-exists? s)))
                     path->string)
                    l)
        string<=? #:key path->string #:cache-keys? #t))

(define (directory-list* pth)
  (directory-list->directory-list* (directory-list pth)))

(define (safely-delete-directory pth)
  ;; Deleting something that is already gone is the point of "safely".
  (swallow 'safely-delete-directory pth
           (lambda () (delete-directory/files pth))
           #:expected? exn:fail:filesystem?)
  (void))

(define (make-parent-directory pth)  
  (define pth-dir (path-only pth))
  (make-directory* pth-dir))

(define (write-to-file* v pth)
  (define tpth (make-temporary-file))
  (write-to-file v tpth #:exists 'truncate)
  (make-parent-directory pth)
  (rename-file-or-directory tpth pth #t))

(define (rebase-path from to)
  (define froms (explode-path from))
  (define froms-len (length froms))
  (lambda (pth)
    (define pths (explode-path pth))
    (apply build-path to (list-tail pths froms-len))))

(define (path->string* pth-string)
  (if (string? pth-string)
      pth-string
      (path->string pth-string)))

;; If `pth` sits under `root`, the path elements below it; otherwise #f.
;;
;; Every caller that relocates a path between two roots needs exactly this,
;; and open-coding it meant four copies of "explode both, compare the first
;; N, strip them" that did not even agree on whether the length guard was
;; `>` or `>=`.
(define (path-prefix-split pth root)
  (define root-parts (explode-path root))
  (define root-len (length root-parts))
  (define pth-parts (explode-path pth))
  (and ((length pth-parts) . >= . root-len)
       (equal? (for/list ([p (in-list pth-parts)]
                          [_ (in-range root-len)])
                 p)
               root-parts)
       (list-tail pth-parts root-len)))

(provide/contract
 [path-prefix-split (path-string? path-string? . -> . (or/c false/c (listof path?)))]
 [current-temporary-directory (parameter/c (or/c false/c path-string?))]
 [safely-delete-directory (path-string? . -> . void)]
 [directory-list->directory-list* ((listof path?) . -> . (listof path?))]
 [directory-list* (path-string? . -> . (listof path?))]
 [write-to-file* (any/c path-string? . -> . void)]
 [make-parent-directory (path-string? . -> . void)]
 [rebase-path (path-string? path-string? . -> . (path-string? . -> . path?))]
 [path->string* (path-string? . -> . string?)])

(module+ test
  (require rackunit)

  (check-equal? (path-prefix-split "/opt/plt/builds/73400/logs" "/opt/plt/builds")
                (list (string->path "73400") (string->path "logs")))
  ;; A root one element shorter still works: nothing depends on the two
  ;; roots having the same depth.
  (check-equal? (path-prefix-split "/extra/builds/55389/logs" "/extra/builds")
                (list (string->path "55389") (string->path "logs")))
  ;; The path may be the root itself.
  (check-equal? (path-prefix-split "/opt/plt/builds" "/opt/plt/builds") '())
  ;; A different prefix of the same length must not match, which is the
  ;; check whose absence let archive lookups resolve to the wrong entry.
  (check-false (path-prefix-split "/opt/plt/other/73400" "/opt/plt/builds"))
  (check-false (path-prefix-split "/elsewhere/73400/logs" "/opt/plt/builds"))
  ;; Shorter than the root.
  (check-false (path-prefix-split "/opt/plt" "/opt/plt/builds")))
