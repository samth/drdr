#lang racket/base
;; The exception that means "this simply is not cached", as opposed to a bug.
;;
;; Classifying that by matching `exn-message` against magic strings coupled
;; every caller to the wording of errors raised in other modules, with no
;; test or compiler protection: rewording one error would have turned every
;; ordinary miss into a reported bug, and a genuine bug whose message
;; happened to contain the right words would have been reported as normal.
(require racket/contract/base)

(struct exn:fail:not-cached exn:fail ())

(define (raise-not-cached fmt . args)
  (raise (exn:fail:not-cached (apply format fmt args) (current-continuation-marks))))

;; A cache lookup legitimately fails when the data is not there, whether
;; that is a missing file or a path the archive does not hold.
(define (not-cached? x)
  (or (exn:fail:not-cached? x) (exn:fail:filesystem? x)))

(provide (struct-out exn:fail:not-cached))
(provide/contract [raise-not-cached (->* (string?) () #:rest (listof any/c) none/c)]
                  [not-cached? (-> any/c boolean?)])

(module+ test
  (require rackunit)

  (check-exn exn:fail:not-cached? (lambda () (raise-not-cached "~a is gone" "x")))
  (check-exn #rx"x is gone" (lambda () (raise-not-cached "~a is gone" "x")))

  ;; Both shapes of "not there" count.
  (check-true (not-cached? (exn:fail:not-cached "m" (current-continuation-marks))))
  (check-true (not-cached? (exn:fail:filesystem "m" (current-continuation-marks))))

  ;; A bug does not, however it is worded.
  (check-false (not-cached? (exn:fail:contract "is not cached" (current-continuation-marks))))
  (check-false (not-cached? (exn:fail "is not in the archive" (current-continuation-marks)))))
