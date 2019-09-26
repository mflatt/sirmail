#lang racket/base
(require racket/date)

(provide make-debug-custodian
         dump-all-thread-traces)

(define original-custodian (current-custodian))

(define custodians (make-weak-hasheq))

(define (make-debug-custodian)
  (define c (make-custodian original-custodian))
  (hash-set! custodians c #t)
  c)

(define (dump-all-thread-traces)
  (call-with-output-file*
   (build-path (if (directory-exists? "/tmp")
                   "/tmp"
                   (find-system-path 'temp-dir))
               "sirmail-threads")
   #:exists 'append
   (lambda (o)
     (define (sep c) (make-string 60 c))
     (fprintf o "~a\n~a\n" (sep #\=) (date->string (seconds->date (current-seconds)) #t))
     (let loop ([l (hash-keys custodians)])
       (cond
         [(null? l) (void)]
         [else
          (define v (car l))
          (cond
            [(custodian? v)
             (loop (append (custodian-managed-list v original-custodian) (cdr l)))]
            [(thread? v)
             (fprintf o "~a\nThread ~a:\n" (sep #\-) (eq-hash-code v))
             (for ([c (in-list (continuation-mark-set->context (continuation-marks v)))])
               (fprintf o "  ~s\n" c))
             (loop (cdr l))]
            [else
             (loop (cdr l))])])))))
