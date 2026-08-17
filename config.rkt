#lang racket/base
(require racket/list
         racket/contract/base
         "cache.rkt"
         "dirstruct.rkt"
         "notify.rkt"
         "scm.rkt"
         "monitor-scm.rkt")

(plt-directory "/opt/plt")
(extra-build-directory "/extra/builds")
(drdr-directory "/opt/svn/drdr")
(git-path "/usr/bin/git")
(Xvfb-path "/usr/bin/Xnest")
(fluxbox-path "/usr/bin/metacity")
(vncviewer-path "/usr/bin/vncviewer")
(current-make-install-timeout-seconds (* 5 60 60))
(current-make-timeout-seconds (* 5 60 60))                                       
(current-subprocess-timeout-seconds 90)
(current-monitoring-interval-seconds 60)
(number-of-cpus 18)

;; `string->number` answers #f rather than raising, so there is nothing here
;; to guard against.
(define string->number* string->number)

(define revisions-b (box #f))

(define (init-revisions!)
  (define builds (directory-list (plt-build-directory)))
  (define nums
    (filter-map
     (compose string->number* path->string)
     builds))
  (define sorted (sort nums <))
  (set-box! revisions-b sorted))

(define (newest-revision)
  (last (unbox revisions-b)))

(define (second-to-last l)
  (list-ref l (- (length l) 2)))

(define (second-newest-revision)
  ;; There may not be two revisions yet, which is not worth reporting.
  (swallow 'second-newest-revision 'revisions
           (lambda () (second-to-last (unbox revisions-b)))
           #:expected? exn:fail:contract?))

(define (newest-completed-revision)
  (define n (newest-revision))
  (if (read-cache* (build-path (revision-dir n) "analyzed"))
      n
      (second-newest-revision)))

(provide/contract
 [revisions-b (box/c (or/c false/c (listof exact-nonnegative-integer?)))]
 [init-revisions! (-> void)]
 [newest-revision (-> exact-nonnegative-integer?)]
 [second-newest-revision (-> (or/c false/c exact-nonnegative-integer?))]
 [newest-completed-revision (-> (or/c false/c exact-nonnegative-integer?))])
