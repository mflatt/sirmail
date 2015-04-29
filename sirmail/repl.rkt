#lang racket/base
(require racket/gui/base
         racket/class
         racket/sandbox)

(provide execute-message-repls)

;; previous evaluation status:
(struct state (texts  ; old result, to enable a no-op shortcut
               cust)) ; custodian to shut down

;; represents a change to message content due to evaluation:
(struct insert (start end editor))

;; Check for changes between ```s, and adjust results in the editor
;; via evaluation as needed
(define (execute-message-repls editor show-error call-as-background start-pos s)
  (define p (open-input-string (substring (send editor get-flattened-text) start-pos)))
  (port-count-lines! p)
  (define-values (texts offsets) (extract-code-blocks p start-pos))
  (cond
   [(and s (equal? texts (state-texts s)))
    ;; No change to code; same state
    s]
   [else
    (define shown? #f) ; show only the first error
    (show-error "")
    
    (when s
      (custodian-shutdown-all (state-cust s)))
    
    (define cust (make-custodian))
    (define insertss
      (parameterize ([current-custodian cust])
        (in-breakable-thread
         call-as-background
         cust
         (lambda ()
           (run-code texts
                     (lambda (e)
                       (define str (send e get-flattened-text))
                       (unless (equal? "" str)
                         (unless shown?
                           (set! shown? #t)
                           (show-error str))))))
         ;; Failure thunk:
         (lambda ()
           (show-error "Stopped")
           #f))))

    (cond
     [insertss
      (apply-changes-to-editor! editor insertss offsets)
      (define new-texts (apply-changes-to-text texts insertss))
      (state new-texts cust)]
     [else
      s])]))
 
;; get all sequences between ```s
(define (extract-code-blocks p start-pos)
  (let loop ([texts null] [offsets null])
    (cond
     [(regexp-match? #rx"(?m:^```)" p)
      (define pos (file-char-position p))
      (define o (open-output-string))
      (regexp-match #rx"(?m:^```)" p 0 #f o)
      (loop (cons (get-output-string o) texts)
            (cons (+ pos start-pos) offsets))]
     [else
      (values (reverse texts) (reverse offsets))])))

;; apply `insert`s to the message editor
(define (apply-changes-to-editor! editor insertss offsets)
  (unless (andmap null? insertss)
    (send editor begin-edit-sequence #f)
    (for ([inserts (in-list (reverse insertss))]
          [offset (in-list (reverse offsets))])
      (for ([insert (in-list (reverse inserts))])
        (define start-pos (+ offset (insert-start insert)))
        (send editor delete start-pos (+ offset (insert-end insert)))
        (define start-at-caret? (= start-pos (send editor get-start-position)))
        (define end-at-caret? (= start-pos (send editor get-end-position)))
        (copy-editor editor (insert-editor insert) start-pos)
        (when (or start-at-caret? end-at-caret?)
          (send editor set-position
                (if start-at-caret?
                    start-pos
                    (send editor get-start-position))
                (if end-at-caret?
                    start-pos
                    (send editor get-end-position))))))
    (send editor end-edit-sequence)))

(define (copy-editor dest src start-pos)
  (let loop ([start-pos start-pos]
             [snip (send src find-first-snip)])
    (when snip
      (send dest insert (send snip copy) start-pos)
      (loop (+ start-pos (send snip get-count))
            (send snip next)))))

(define (indent-output editor indent)
  (unless (zero? indent)
    (let loop ([para 0] [pos -1])
      (define new-pos (send editor paragraph-start-position para))
      (unless (or (= new-pos pos)
                  (= new-pos (send editor last-position)))
        (send editor insert (make-string indent #\space) new-pos)
        (loop (add1 para) new-pos))))
  (send editor insert "\n" 0))

;; apply `insert`s to the extracted text, for the no-op shortcut
(define (apply-changes-to-text texts insertss)
  (for/list ([text (in-list texts)]
             [inserts (in-list insertss)])
    (for/fold ([text text]) ([insert (in-list (reverse inserts))])
      (string-append (substring text 0 (min (string-length text) (insert-start insert)))
                     (send (insert-editor insert) get-flattened-text)
                     (substring text (min (string-length text) (insert-end insert)))))))

;; evaluate ``` blocks; when we find a module, then
;; create a new evaluator
(define (run-code texts show-error)
  (define cust (make-custodian))
  (let loop ([texts texts] [e #f])
    (cond
     [(null? texts) null]
     [else
      (define text (car texts))
      (define-values (new-e repl)
        (cond
         [(regexp-match? #px"\\s*#lang" text)
          ;; make a new evaluator; nothing to eval afterward
          (when e (kill-evaluator e))
          (define-values (evals stderr)
            (do-eval #f (lambda ()
                          (call-with-sandbox-config
                           (lambda ()
                             (install-printer (make-module-evaluator text)))))))
          (show-error stderr)
          (values (and (not (void? evals))
                       (car evals))
                  #f)]
         [(not e)
          ;; create a default evaluator
          (values
           (call-with-sandbox-config
            (lambda ()
              (install-printer (make-evaluator 'racket))))
           text)]
         [else
          ;; continue using the evaluator
          (values e text)]))
      ;; When we have a REPL-style ``` block, look for prompts
      ;; and evaluate expressions after them:
      (define inserts
        (cond
         [repl
          (define i (open-input-string text))
          (port-count-lines! i)
          (let loop ()
            (define m (regexp-match #px"^\\s*>(\\s*)" i))
            (cond
             [(and m (not (regexp-match? #rx"\n" (cadr m))))
              ;; Found a prompt with more before a newline
              (define-values (gt-line gt-col gt-pos) (port-next-location i))
              (define indent (max 0 (- gt-col (string-length (bytes->string/utf-8 (cadr m))) 1)))
              (define-values (exprs stderr) (do-eval #f (lambda () (read i))))
              (show-error stderr)
              (cond
               [(or (void? exprs)
                    (eof-object? (car exprs)))
                ;; No expression to evaluate, so no changes
                null]
               [else
                (define expr (car exprs))
                (define start-pos (file-char-position i))
                (cond
                 [(regexp-match-peek #px"^[^\n]*[^\\s][^\n]*\n" i)
                  ;; Found non-whitespace on line where the expression ends;
                  ;; probably it's being edited, and we shouldn't discard text
                  null]
                 [else
                  ;; Evaluate and set up `insert` record to replace old
                  ;; content with new content
                  (define-values (results out) (do-eval new-e
                                                        (lambda ()
                                                          (new-e (datum->syntax #f expr)))))
                  (define m (regexp-match #px"\n\\s*(?:(?=>)|`*$)" i))
                  (define end-pos (file-char-position i))
                  (indent-output out indent)
                  (cons (insert start-pos
                                (if m
                                    (- end-pos (sub1 (string-length (bytes->string/utf-8 (car m)))))
                                    end-pos)
                                out)
                        (loop))])])]
             [m
              ;; Found an empty prompt; skip it
              (loop)]
             [else null]))]
         [else
          null]))
      (cons inserts (loop (cdr texts) new-e))])))

;; ----------------------------------------
;; Evaluation helpers

(define (call-with-sandbox-config thunk)
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output current-output-port]
                    [sandbox-propagate-breaks #f])
       (thunk)))))

(define (do-eval e thunk)
  (define editor (new text%))
  (define o (open-output-text-editor editor #:eventspace #f))
  (values
   (with-handlers ([void (lambda (exn)
                           (if (exn? exn)
                               (fprintf o "~a\n" (exn-message exn))
                               (fprintf o "~s\n" exn)))])
     (when e
       (call-in-sandbox-context e (lambda ()
                                    (current-output-port o)
                                    (current-error-port o))))
     (define vs (call-with-values thunk list))
     (when e
       (call-in-sandbox-context e (lambda ()
                                    (map (current-print) vs)))
       (display (get-output e) o))
     vs)
   editor))

(define (file-char-position i)
  (define-values (line col pos) (port-next-location i))
  (sub1 pos))

;; ----------------------------------------
;; Pretty printing

(define (install-printer e)
  (call-in-sandbox-context e (lambda ()
                               (current-print
                                (dynamic-require 'racket/pretty 'pretty-print-handler))
                               ((dynamic-require 'racket/pretty 'pretty-print-size-hook)
                                (lambda (v display? p)
                                  (if (and (port-writes-special? p)
                                           (v . is-a? . snip%))
                                      1
                                      #f)))
                               ((dynamic-require 'racket/pretty 'pretty-print-print-hook)
                                (lambda (v display? p)
                                  (write-special v p)))))
  e)

;; ----------------------------------------
;; Breakabale

(define (in-breakable-thread call-as-background cust work fail)
  (define result #f)
  (with-handlers ([exn:break? (lambda (exn)
                                (custodian-shutdown-all cust))])
    (call-as-background
     (lambda (break-bad break-ok)
       (define sub-cust (make-custodian))
       (with-handlers ([exn:break? (lambda (exn)
                                     (custodian-shutdown-all sub-cust)
                                     (set! result fail)
                                     (raise exn))])
         (break-ok)
         (define t
           (parameterize ([current-custodian sub-cust])
             (thread (lambda ()
                       (with-handlers ([void (lambda (exn) (set! result (box exn)))])
                         (set! result (call-with-values work list)))))))
         (sync t)
         (break-bad)))))
  (cond
   [(procedure? result) (result)]
   [(box? result)
    (raise (unbox result))]
   [else (apply values result)]))
