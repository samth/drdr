#lang racket/base
(require racket/port
         racket/file
         racket/contract/base
         "path-utils.rkt")

; (symbols 'always 'cache 'no-cache)
(define cache/file-mode (make-parameter 'cache))
(define (cache/file pth thnk)
  (define mode (cache/file-mode))
  (define (recompute!)
    (define v (thnk))
    (write-cache! pth v)
    v)
  (case mode
    [(always) (recompute!)]
    [(cache no-cache)
     (with-handlers 
         ([exn:fail?
           (lambda (x)
             (case mode
               [(no-cache) (raise-not-cached "cache/file: No cache available: ~a" pth)]
               [(cache always)
                #;(printf "cache/file: running ~S for ~a\n" thnk pth)
                (recompute!)]))])
       (read-cache pth))]))

(define (cache/file/timestamp pth thnk)
  (cache/file 
   pth
   (lambda ()
     (thnk)
     (current-seconds)))
  (void))

(require "archive.rkt"
         "dirstruct.rkt"
         "notify.rkt"
         "not-cached.rkt")

;; Anything that is not an ordinary miss -- a contract violation, a
;; malformed archive -- is a bug, and reporting it as a miss is how
;; `path->revision` silently mistook every archived build for a missing one.
(define (miss-on-failure who pth thunk)
  (swallow who pth thunk #:expected? not-cached?))

;; An archive records the paths its build had when it was created, which is
;; not necessarily where the build is now: it may have aged out to the extra
;; directory, or been re-archived there.  `pth` is expressed relative to the
;; build's current directory, so say so rather than leaving the archive's own
;; recorded root to stand in for it.
(define (consult-archive pth)
  (define rev (path->revision pth))
  (define file-bytes
    (archive-extract-file (revision-archive rev) pth #:base (revision-dir rev)))
  (with-input-from-bytes file-bytes read))

(define (consult-archive/directory-list* pth)
  (define rev (path->revision pth))
  (directory-list->directory-list*
   (archive-directory-list (revision-archive rev) pth #:base (revision-dir rev))))

(define (consult-archive/directory-exists? pth)
  (define rev (path->revision pth))
  (archive-directory-exists? (revision-archive rev) pth #:base (revision-dir rev)))

(define (cached-directory-list* dir-pth)
  (if (directory-exists? dir-pth)
      (directory-list* dir-pth)
      (or (miss-on-failure 'cached-directory-list* dir-pth
                           (lambda () (consult-archive/directory-list* dir-pth)))
          (raise-not-cached "cached-directory-list*: not cached: ~e" dir-pth))))

(define (cached-directory-exists? dir-pth)
  (if (file-exists? dir-pth)
      #f
      (or (directory-exists? dir-pth)
          (miss-on-failure 'cached-directory-exists? dir-pth
                           (lambda () (consult-archive/directory-exists? dir-pth))))))

(define (read-cache pth)
  (if (file-exists? pth)
      (file->value pth)
      (or (miss-on-failure 'read-cache pth
                           (lambda () (consult-archive pth)))
          (raise-not-cached "read-cache: File is not cached: ~e" pth))))
(define (read-cache* pth)
  ;; `read-cache` reports the archive branch itself, but `file->value` on a
  ;; truncated or corrupt cache file raises something that is not a miss at
  ;; all, and this used to turn that into #f without a word.
  (miss-on-failure 'read-cache* pth (lambda () (read-cache pth))))
(define (write-cache! pth v)
  (write-to-file* v pth))
(define (delete-cache! pth)
  (swallow 'delete-cache! pth
           (lambda () (delete-file pth))
           #:expected? exn:fail:filesystem?)
  (void))

(provide/contract
 [cache/file-mode (parameter/c (symbols 'always 'cache 'no-cache))]
 [cache/file (path-string? (-> any/c) . -> . any/c)]
 [cache/file/timestamp (path-string? (-> void) . -> . void)]
 [cached-directory-list* (path-string? . -> . (listof path-string?))]
 [cached-directory-exists? (path-string? . -> . boolean?)]
 [read-cache (path-string? . -> . any/c)]
 [read-cache* (path-string? . -> . any/c)]
 [write-cache! (path-string? any/c . -> . void)]
 [delete-cache! (path-string? . -> . void)])

(module+ test
  (require rackunit
           (submod "notify.rkt" test-support))

  ;; An absent file is an ordinary miss and must stay quiet: this is the
  ;; common case for revisions that have no build directory at all.
  (check-equal? (warnings-during
                 (lambda ()
                   (check-false
                    (miss-on-failure 'test "/no/such/file"
                                     (lambda ()
                                       (call-with-input-file "/no/such/file" read))))))
                '())

  ;; So is a path the archive genuinely does not contain.
  (check-equal? (warnings-during
                 (lambda ()
                   (miss-on-failure 'test "/x"
                                    (lambda ()
                                      (raise-not-cached "~e is not in the archive" "/x")))))
                '())

  ;; Classification does not depend on how an error is worded: a bug whose
  ;; message happens to read like a miss is still reported.
  (check-equal? (length
                 (warnings-during
                  (lambda ()
                    (miss-on-failure 'test "/x"
                                     (lambda ()
                                       (error 'oops "is not in the archive"))))))
                1)

  ;; A contract violation -- the shape `path->revision` used to raise for
  ;; every archived build -- is a bug, so it must be reported even though
  ;; the caller still gets #f.
  (let ([msgs (warnings-during
               (lambda ()
                 (check-false
                  (miss-on-failure 'test "/extra/builds/1/logs"
                                   (lambda ()
                                     (raise (exn:fail:contract
                                             "path->revision: broke its own contract"
                                             (current-continuation-marks))))))))])
    (check-equal? (length msgs) 1)
    (check-regexp-match #rx"swallowed exception" (car msgs))
    (check-regexp-match #rx"broke its own contract" (car msgs))
    (check-regexp-match #rx"/extra/builds/1/logs" (car msgs))))
