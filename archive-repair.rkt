#lang racket/base
(require racket/cmdline
         "config.rkt"
         "archive.rkt"
         "path-utils.rkt"
         "dirstruct.rkt"
         "make-archive-lib.rkt")

(init-revisions!)

(define rev
  (command-line #:program "archive-repair"
                #:args (n) (string->number n)))

(when (file-exists? (revision-archive rev))
  ;; The archive names its contents by the paths the build had when it was
  ;; created, which need not be where it lives now; `#:base` says which
  ;; directory the paths passed here are relative to.
  (archive-extract-to (revision-archive rev)
                      (revision-dir rev)
                      (revision-dir rev)
                      #:base (revision-dir rev))
  (delete-file (revision-archive rev))
  (make-archive rev))
