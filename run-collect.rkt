#lang racket/base
(require racket/local
         racket/match
         racket/list
         racket/contract/base
         racket/async-channel
         "status.rkt"
         "notify.rkt"
         "rewriting.rkt"
         "dirstruct.rkt"
         "cache.rkt")

(define (command+args+env->command+args 
         #:env env
         cmd args)
  (values "/usr/bin/env"
          (append (for/list ([(k v) (in-hash env)])
                    (format "~a=~a" k v))
                  (list* cmd
                         args))))

(define (run/collect/wait 
         #:env env
         #:timeout timeout
         command args)
  (define start-time 
    (current-inexact-milliseconds))
  
  ; Run the command
  (define-values (new-command new-args)
    (command+args+env->command+args
     #:env env
     command args))
  (define command-line
    (list* command args))

  (notify! "Running: ~a ~S" command args)

  (define-values
    (the-process stdout stdin stderr)
    (parameterize ([subprocess-group-enabled #t])
      (apply subprocess
             #f #f #f
             new-command 
             new-args)))
  ; Run it without input
  (close-output-port stdin)
  ; Wait for all the output and the process death or timeout
  (local
    [(define the-alarm
       (alarm-evt (+ start-time (* 1000 timeout))))

     (define line-ch (make-async-channel))
     (define (read-port-t make port)
       (thread
        (λ ()
          (let loop ()
            (define l (read-bytes-line port))
            (if (eof-object? l)
                (async-channel-put line-ch l)
                (begin (async-channel-put line-ch (make l))
                       (loop)))))))
     (define stdout-t (read-port-t make-stdout stdout))
     (define stderr-t (read-port-t make-stderr stderr))
     
     (define final-status
       (let loop ([open-ports 2]
                  [end-time #f]
                  [status #f]
                  [log empty])
         (define process-done? (and end-time #t))
         (define output-done? (zero? open-ports))
         (if (and output-done? process-done?)
             (if status
                 (if (= status 2)
                   (make-timeout start-time end-time command-line (reverse log))
                   (make-exit start-time end-time command-line (reverse log) status))
                 (make-timeout start-time end-time command-line (reverse log)))
             (sync (if process-done?
                       never-evt
                       (choice-evt 
                        (handle-evt the-alarm
                                    (λ (_)
                                      (define end-time 
                                        (current-inexact-milliseconds))
                                      (subprocess-kill the-process #f)
                                      ;; Sleep for 10% of the timeout
                                      ;; before sending the death
                                      ;; signal
                                      (sleep (* timeout 0.1))
                                      (subprocess-kill the-process #t)
                                      (loop open-ports end-time status log)))
                        (handle-evt the-process
                                    (λ (_)
                                      (define end-time 
                                        (current-inexact-milliseconds))
                                      (loop open-ports end-time (subprocess-status the-process) log)))))
                   (if output-done?
                       never-evt
                       (handle-evt line-ch
                                   (lambda (l)
                                   (match l
                                     [(? eof-object?)
                                      (loop (sub1 open-ports) end-time status log)]
                                     [l
                                      (loop open-ports end-time status (list* l log))]))))))))]
    
    (close-input-port stdout)
    (close-input-port stderr)
    
    (notify! "Done: ~a ~S" command args)
    
    final-status))

(define-syntax regexp-replace**
  (syntax-rules ()
    [(_ () s) s]
    [(_ ([pat0 subst0]
         [pat subst]
         ...)
        s)
     (regexp-replace* (regexp-quote pat0)
                      (regexp-replace** ([pat subst] ...) s)
                      subst0)]))

;; The build directory for a revision, and the same directory with the
;; revision replaced by its placeholder.  Both come from one read of the
;; build directory, so the prefix being replaced and the prefix replacing it
;; cannot disagree.
(define (revision-build-paths rev)
  (define builds (plt-build-directory))
  (values (path->string (build-path builds (number->string rev)))
          (path->string (build-path builds "<current-rev>"))))

;; Scrub machine- and push-specific paths out of captured output so that a
;; log only changes when the test's behaviour does.  The revision is
;; anchored to the build directory: substituting the bare number rewrote
;; every other occurrence of those digits too, so a push whose number
;; appeared inside a git checksum -- as 73506 does in expeditor's -- made
;; `raco pkg show` report a spurious change for that package.
(define (scrub-output s rev-dir rev-dir/scrubbed tmp home cwd)
  (regexp-replace** ([rev-dir rev-dir/scrubbed]
                     [tmp "<tmp>"]
                     [home "<home>"]
                     [cwd "<cwd>"])
                    s))

(define (run/collect/wait/log log-path command
                              #:timeout timeout
                              #:env env
                              args)
  (define ran? #f)
  (cache/file
   log-path
   (lambda ()
     (notify! "No cache: ~a" log-path)

     (define-values (rev-dir rev-dir/scrubbed)
       (revision-build-paths (current-rev)))
     (define home (hash-ref env "HOME"))
     (define tmp (hash-ref env "TMPDIR"))
     (define cwd (path->string (current-directory)))
     (define (rewrite s)
       (scrub-output s rev-dir rev-dir/scrubbed tmp home cwd))
     
     (set! ran? #t)
     (rewrite-status
      #:rewrite rewrite
      (run/collect/wait
       #:timeout timeout
       #:env env
       command args))))
  ran?)

(provide/contract
 [command+args+env->command+args 
  (string? (listof string?) #:env (hash/c string? string?) . -> . (values string? (listof string?)))]
 [run/collect/wait
  (string? 
   #:env (hash/c string? string?) 
   #:timeout exact-nonnegative-integer? 
   (listof string?) 
   . -> . status?)]
 [run/collect/wait/log 
  (path-string? string? 
                #:env (hash/c string? string?) 
                #:timeout exact-nonnegative-integer?
                (listof string?)
                . -> . boolean?)])

(module+ test
  (require rackunit)

  (parameterize ([plt-directory "/opt/plt"])
    (define-values (rev-dir rev-dir/scrubbed) (revision-build-paths 73506))
    ;; `run/collect/wait/log` passes (path->string (current-directory)), which
    ;; always ends in a separator, so pin that shape rather than one the
    ;; caller never produces.
    (define cwd "/opt/plt/builds/73506/trunk/")
    (define (scrub s)
      (scrub-output s rev-dir rev-dir/scrubbed "/tmp/x/" "/home/jay" cwd))

    (check-true (regexp-match? #rx"/$" (path->string (current-directory))))

    ;; Build paths still collapse to the placeholder, exactly as before.
    (check-equal? (scrub "/opt/plt/builds/73506/logs/pkgs/base")
                  "/opt/plt/builds/<current-rev>/logs/pkgs/base")
    (check-equal? (scrub "/tmp/x/foo") "<tmp>foo")
    (check-equal? (scrub "/home/jay/.racket") "<home>/.racket")
    (check-equal? (scrub "/opt/plt/builds/73506/trunk/racket") "<cwd>racket")

    ;; A checksum that happens to contain the push number must survive:
    ;; this is expeditor's real checksum on push 73506, which used to be
    ;; reported as changed on that push alone.
    (check-equal? (scrub "65e20a410bdc5f09c0682a1bb57cac2b68d73506")
                  "65e20a410bdc5f09c0682a1bb57cac2b68d73506")
    ;; and so must a bare mention of the number
    (check-equal? (scrub "ran 73506 tests") "ran 73506 tests")))
