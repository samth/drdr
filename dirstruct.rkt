#lang racket/base
(require racket/bool
         racket/contract/base
         "path-utils.rkt"
         "not-cached.rkt")

(define number-of-cpus
  (make-parameter 1))

(define current-subprocess-timeout-seconds
  (make-parameter (* 60 10)))

(define plt-directory
  (make-parameter (build-path (current-directory))))

(define (plt-build-directory)
  (build-path (plt-directory) "builds"))

(define (plt-future-build-directory)
  (build-path (plt-directory) "future-builds"))

(define (plt-new-pushes-file)
  (build-path (plt-directory) "pushes"))

(define (plt-data-directory)
  (build-path (plt-directory) "data"))

(define drdr-directory
  (make-parameter (build-path (current-directory) "drdr")))

(define make-path
  (make-parameter "/usr/bin/make"))
(define tar-path
  (make-parameter "/bin/tar"))
(define bash-path
  (make-parameter "/bin/bash"))

(define Xvfb-path
  (make-parameter "/usr/bin/Xvfb"))

(define fluxbox-path
  (make-parameter "/usr/bin/fluxbox"))

(define vncviewer-path
  (make-parameter "/usr/bin/vncviewer"))

(define (plt-repository)
  (build-path (plt-directory) "repo"))

(define current-make-timeout-seconds
  (make-parameter (* 60 30)))

(define current-make-install-timeout-seconds
  (make-parameter (* 60 30)))

(define current-rev
  (make-parameter #f))

(define previous-rev
  (make-parameter #f))

(define extra-build-directory
  (make-parameter #f))

(define (revision-dir rev)
  (define primary (build-path (plt-build-directory) (number->string rev)))
  (if (or (directory-exists? primary)
          (not (extra-build-directory)))
      primary
      (let ([extra (build-path (extra-build-directory) (number->string rev))])
        (if (directory-exists? extra)
            extra
            primary))))

;; Replace the prefix of a path that was stored with the primary build
;; directory prefix so it works with the extra build directory.
(define (relocate-build-path pth)
  (define pth* (if (path? pth) pth (string->path pth)))
  (define below (and (extra-build-directory)
                     (not (file-exists? pth*))
                     (not (directory-exists? pth*))
                     (path-prefix-split pth* (plt-build-directory))))
  (cond
    [(not (pair? below)) pth*]
    [else
     (define relocated (apply build-path (extra-build-directory) below))
     (if (or (file-exists? relocated) (directory-exists? relocated))
         relocated
         pth*)]))

(define (revision-log-dir rev)
  (build-path (revision-dir rev) "logs"))

(define (revision-analyze-dir rev)
  (build-path (revision-dir rev) "analyze"))

(define (revision-trunk-dir rev)
  (build-path (revision-dir rev) "trunk"))
(define (revision-trunk.tgz rev)
  (build-path (revision-dir rev) "trunk.tgz"))
(define (revision-trunk.tar.7z rev)
  (build-path (revision-dir rev) "trunk.tar.7z"))

(define (revision-commit-msg rev)
  (build-path (revision-dir rev) "commit-msg"))

;; A build lives under the primary build directory until it ages out, and
;; under the extra build directory afterwards.  Those two roots need not
;; have the same number of path elements, so find the revision by matching
;; a root prefix rather than by indexing at the primary root's length.
(define (path->revision pth)
  (define (revision-under root)
    (define below (and root (path-prefix-split pth root)))
    (and (pair? below)
         (string->number (path->string* (car below)))))
  (or (revision-under (plt-build-directory))
      (revision-under (extra-build-directory))
      ;; Reachable for paths that are simply outside both build roots --
      ;; `future-record-path`, say -- so callers standing a default in for
      ;; uncached data must be able to tell this from a bug.
      (raise-not-cached "path->revision: no revision in ~e" pth)))

(define (revision-archive rev)
  (build-path (revision-dir rev) "archive.db"))

(define (future-record-path n)
  (build-path (plt-future-build-directory) (number->string n)))

(define (path-timing-log p)
  (path-add-suffix (build-path (plt-data-directory) p) #".timing"))

(define (path-timing-png p)
  (path-add-suffix (path-timing-log p) #".png"))
(define (path-timing-html p)
  (path-add-suffix (path-timing-log p) #".html"))
(define (path-timing-png-prefix p)
  (path-timing-log p))

(define build? (make-parameter #t))

(define (on-unix?)
  (symbol=? 'unix (system-type 'os)))

(provide/contract
 [current-subprocess-timeout-seconds (parameter/c exact-nonnegative-integer?)]
 [number-of-cpus (parameter/c exact-nonnegative-integer?)]
 [current-rev (parameter/c (or/c false/c exact-nonnegative-integer?))]
 [previous-rev (parameter/c (or/c false/c exact-nonnegative-integer?))]
 [plt-directory (parameter/c path-string?)]
 [plt-build-directory (-> path?)]
 [extra-build-directory (parameter/c (or/c false/c path-string?))]
 [plt-data-directory (-> path?)]
 [plt-future-build-directory (-> path?)]
 [drdr-directory (parameter/c path-string?)]
 [tar-path (parameter/c (or/c false/c string?))]
 [bash-path (parameter/c (or/c false/c string?))]
 [make-path (parameter/c (or/c false/c string?))]
 [Xvfb-path (parameter/c (or/c false/c string?))]
 [vncviewer-path (parameter/c (or/c false/c string?))] 
 [fluxbox-path (parameter/c (or/c false/c string?))]
 [build? (parameter/c boolean?)]
 [on-unix? (-> boolean?)]
 [plt-repository (-> path?)]
 [path-timing-log (path-string? . -> . path?)]
 [path-timing-png (path-string? . -> . path?)]
 [path-timing-png-prefix (path-string? . -> . path?)]
 [path-timing-html (path-string? . -> . path?)]
 [future-record-path (exact-nonnegative-integer? . -> . path?)]
 [current-make-timeout-seconds (parameter/c exact-nonnegative-integer?)]
 [current-make-install-timeout-seconds (parameter/c exact-nonnegative-integer?)]
 [relocate-build-path (path-string? . -> . path?)]
 [revision-dir (exact-nonnegative-integer? . -> . path?)]
 [revision-commit-msg (exact-nonnegative-integer? . -> . path?)]
 [revision-log-dir (exact-nonnegative-integer? . -> . path?)]
 [revision-analyze-dir (exact-nonnegative-integer? . -> . path-string?)]
 [revision-trunk-dir (exact-nonnegative-integer? . -> . path?)]
 [revision-trunk.tgz (exact-nonnegative-integer? . -> . path?)]
 [revision-trunk.tar.7z (exact-nonnegative-integer? . -> . path?)]
 [revision-archive (exact-nonnegative-integer? . -> . path?)]
 [path->revision (path-string? . -> . exact-nonnegative-integer?)]
 [plt-new-pushes-file (-> path-string?)])

(module+ test
  (require rackunit)

  (define (with-roots primary extra thunk)
    (parameterize ([plt-directory primary]
                   [extra-build-directory extra])
      (thunk)))

  ;; The extra root has one FEWER element than the primary one here, which
  ;; is the layout in production: indexing at the primary root's length used
  ;; to land on "logs" and produce #f, so every archived build looked absent.
  (with-roots
   "/opt/plt" "/extra/builds"
   (lambda ()
     (check-equal? (path->revision "/opt/plt/builds/73400/logs") 73400)
     (check-equal? (path->revision "/opt/plt/builds/73400/logs/pkgs/base") 73400)
     (check-equal? (path->revision "/extra/builds/55389/logs") 55389)
     (check-equal? (path->revision "/extra/builds/55389/archive.db") 55389)
     (check-equal? (path->revision "/extra/builds/55389/logs/pkgs/base") 55389)
     (check-exn exn:fail? (lambda () (path->revision "/somewhere/else/55389/logs")))))

  ;; An extra root longer than the primary one must work the same way, so
  ;; that nothing depends on the two roots' relative depth.
  (with-roots
   "/opt/plt" "/mnt/a/b/c/builds"
   (lambda ()
     (check-equal? (path->revision "/mnt/a/b/c/builds/50001/logs") 50001)
     (check-equal? (path->revision "/opt/plt/builds/73400/logs") 73400)))

  ;; With no extra directory configured, only the primary root resolves.
  (with-roots
   "/opt/plt" #f
   (lambda ()
     (check-equal? (path->revision "/opt/plt/builds/73400/logs") 73400)
     (check-exn exn:fail? (lambda () (path->revision "/extra/builds/55389/logs")))))

  ;; A path that stops at the revision itself has no revision element after
  ;; the root, and must not be read as one.
  (with-roots
   "/opt/plt" "/extra/builds"
   (lambda ()
     (check-exn exn:fail? (lambda () (path->revision "/opt/plt/builds")))))

  ;; A path outside both roots is a miss, not a bug: callers stand a default
  ;; in for it, and must be able to tell the two apart.
  (with-roots
   "/opt/plt" "/extra/builds"
   (lambda ()
     (check-exn exn:fail:not-cached?
                (lambda () (path->revision "/opt/plt/future-builds/1234"))))))
