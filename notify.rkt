#lang racket/base
(require racket/contract/base
         racket/date)

(define (notify! fmt . args)
  (define now (current-seconds))
  (log-info (format "[~a: ~a] ~a" now (seconds->string now) (apply format fmt args))))

(define (seconds->string secs)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (seconds->date secs) #t)))

(define-logger drdr)

;; Run `thunk`, substituting `on-fail` if it raises.
;;
;; Discarding an exception silently makes a bug indistinguishable from the
;; ordinary situation the fallback stands in for, which is how a contract
;; violation in `path->revision` passed for a cache miss.  So report
;; anything `expected?` does not account for.  `who` and `context` say
;; where it happened; `expected?` describes the failures that are part of
;; normal operation, and defaults to none of them.
(define (swallow who context thunk
                 #:expected? [expected? (lambda (x) #f)]
                 #:on-fail [on-fail (lambda () #f)])
  (with-handlers ([exn:fail?
                   (lambda (x)
                     (unless (expected? x)
                       (log-drdr-warning "~a: swallowed exception for ~e: ~a"
                                         who context (exn-message x)))
                     (on-fail))])
    (thunk)))

(provide/contract
 [seconds->string (-> number? string?)]
 [notify! ((string?) () #:rest (listof any/c) . ->* . void)]
 [swallow (->* (any/c any/c (-> any))
               (#:expected? (-> exn:fail? boolean?) #:on-fail (-> any))
               any)])

(module test-support racket/base
  (require racket/logging)
  (provide warnings-during)
  ;; Collect the drdr warnings logged while `thunk` runs.  Shared so that a
  ;; change to the topic or level cannot leave one test suite silently
  ;; observing nothing.
  (define (warnings-during thunk)
    (define msgs '())
    (with-intercepted-logging
      (lambda (l) (set! msgs (cons (vector-ref l 1) msgs)))
      thunk
      'warning 'drdr)
    (reverse msgs)))

(module+ test
  (require rackunit
           (submod ".." test-support))

  (seconds->string (current-seconds))

  ;; A thunk that returns normally is left alone.
  (check-equal? (swallow 'test "ctx" (lambda () 'ok)) 'ok)

  ;; An unexpected failure is reported, and the fallback is returned.
  (let ([msgs (warnings-during
               (lambda ()
                 (check-equal? (swallow 'test "ctx"
                                        (lambda () (error 'boom "went wrong"))
                                        #:on-fail (lambda () 'fallback))
                               'fallback)))])
    (check-equal? (length msgs) 1)
    (check-regexp-match #rx"test" (car msgs))
    (check-regexp-match #rx"ctx" (car msgs))
    (check-regexp-match #rx"went wrong" (car msgs)))

  ;; A failure the caller accounts for is silent.
  (check-equal? (warnings-during
                 (lambda ()
                   (swallow 'test "ctx"
                            (lambda () (raise (exn:fail:filesystem
                                               "gone" (current-continuation-marks))))
                            #:expected? exn:fail:filesystem?)))
                '())

  ;; `on-fail` runs per call, so a fresh value is not shared between them.
  (let ([a (swallow 'test "ctx" (lambda () (error "x")) #:on-fail make-hash)]
        [b (swallow 'test "ctx" (lambda () (error "x")) #:on-fail make-hash)])
    (check-false (eq? a b)))

  ;; Only exn:fail? is caught; anything else -- a break, a raised value --
  ;; still escapes.
  (check-exn (lambda (x) (eq? x 'not-a-failure))
             (lambda ()
               (swallow 'test "ctx" (lambda () (raise 'not-a-failure))))))
