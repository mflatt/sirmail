#lang racket/base
(require racket/gui/base
         racket/class
         racket/sandbox
         syntax/modread
         pict
         pict/snip)

(provide execute-message-repls)

;; previous evaluation status:
(struct state (texts  ; old result, to enable a no-op shortcut
               kinds  ; also old result
               cust)) ; custodian to shut down

;; represents a change to message content due to evaluation:
(struct insert (start end editor new?))

;; Check for changes between ```s, and adjust results in the editor
;; via evaluation as needed
(define (execute-message-repls editor show-error call-as-background start-pos s)
  (define p (open-input-string (substring (send editor get-flattened-text) start-pos)))
  (port-count-lines! p)
  (define-values (texts kinds offsets) (extract-code-blocks p start-pos))
  (cond
   [(and s
         (equal? texts (state-texts s))
         (equal? kinds (state-kinds s)))
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
                     kinds
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
      (define-values (new-texts new-kinds) (apply-changes-to-text texts kinds insertss))
      (state new-texts new-kinds cust)]
     [else
      s])]))
 
;; get all sequences between ```s, and return a list of text
;; and a parallel list of kinds, where a kind is '#:code, '#:keyword,
;; or a module path (which implies '#:code)
(define (extract-code-blocks p start-pos)
  (let loop ([texts null] [kinds null] [offsets null] [output? #f])
    (cond
     [(and (not output?) (regexp-try-match #rx"^Output:\n(?m:```.*)" p))
      (loop texts kinds offsets #t)]
     [(or output? (regexp-try-match #rx"^(?m:```.*)" p))
      => (lambda (m)
           ;; extended Markdown fenced-block syntax is
           ;;   ``` <language> <filename> ....
           ;; So, recognize `racket` followed by <filename> as a module decl
           ;; for <filename>, where <filename> is more generally a module path
           (define module-name
             (and (pair? m)
                  (let ([m (regexp-match #rx"^``` *racket ([^ ]+)" (car m))])
                    (and m (with-handlers ([exn:fail:read? (lambda (exn) #f)])
                             (read (open-input-bytes (cadr m))))))))
           (define pos (file-char-position p))
           (define o (open-output-string))
           (regexp-match #rx"(?m:^```.*)" p 0 #f o)
           (loop (cons (get-output-string o) texts)
                 (cons (or module-name
                           (if output?
                               '#:output
                               '#:code))
                       kinds)
                 (cons (+ pos start-pos) offsets)
                 #f))]
     [(eof-object? (read-line p))
      (values (reverse texts) (reverse kinds) (reverse offsets))]
     [else
      (loop texts kinds offsets #f)])))

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
        (if (string? (insert-editor insert))
            (send editor insert (insert-editor insert) start-pos)
            (copy-editor editor (insert-editor insert) start-pos))
        (when (or start-at-caret? end-at-caret?)
          (send editor set-position
                (if start-at-caret?
                    start-pos
                    (send editor get-start-position))
                (if end-at-caret?
                    start-pos
                    (send editor get-end-position))))))
    (send editor end-edit-sequence)))

;; copy editor content to move output to mail message
(define (copy-editor dest src start-pos)
  (let loop ([start-pos start-pos]
             [snip (send src find-first-snip)])
    (when snip
      (send dest insert (send snip copy) start-pos)
      (loop (+ start-pos (send snip get-count))
            (send snip next)))))

;; indent error messages, etc., to match the prompt location
(define (indent-output editor indent)
  (unless (zero? indent)
    (let loop ([para 0] [pos -1])
      (define new-pos (send editor paragraph-start-position para))
      (unless (or (= new-pos pos)
                  (= new-pos (send editor last-position)))
        (send editor insert (make-string indent #\space) new-pos)
        (loop (add1 para) new-pos))))
  (send editor insert "\n" 0))

;; apply `insert`s to the extracted text, in support of the no-op shortcut;
;;  output blocks needs to be matched up with existing output blocks, with
;;  new output blocks generated as needed
(define (apply-changes-to-text texts kinds insertss)
  (let loop ([texts texts] [kinds kinds] [insertss insertss] [new-texts null] [new-kinds null])
    (cond
     [(null? texts)
      (values (reverse new-texts) (reverse new-kinds))]
     [else
      (define text (car texts))
      (define inserts (car insertss))
      (cond
       [(and (pair? inserts)
             (insert-new? (car inserts)))
        (unless (= (length inserts) 2) (error "expected 2 parts"))
        (loop (cdr texts)
              (cdr kinds)
              (cdr insertss)
              (cons (send (insert-editor (cadr inserts)) get-flattened-text)
                    (cons (car texts)
                          new-texts))
              (cons '#:output (cons '#:code new-kinds)))]
       [else
        (define new-text
          (for/fold ([text text]) ([insert (in-list (reverse inserts))])
            (string-append (substring text 0 (min (string-length text) (insert-start insert)))
                           (if (string? (insert-editor insert))
                               (insert-editor insert)
                               (send (insert-editor insert) get-flattened-text))
                           (substring text (min (string-length text) (insert-end insert))))))
        (loop (cdr texts)
              (cdr kinds)
              (cdr insertss)
              (cons new-text new-texts)
              (cons (car kinds) new-kinds))])])))

;; evaluate ``` blocks; create a new evaluator whenever we have a module
;; block with no name for the module (in which case we call the module
;; `program'), while REPL-style blocks continue with the namespace of
;; the preceding module (or `racket` if no preceding)
(define (run-code texts kinds show-error)
  (define cust (make-custodian))
  (define declared-modules (make-hash))
  (let loop ([texts texts] [kinds kinds] [e #f])
    (cond
     [(null? texts)
      (when e (pump-evaluator e))
      null]
     [(eq? '#:output (car kinds))
      ;; Not preceded by a module block, so empty it
      (define text (car texts))
      (define inserts
        (if (equal? text "\n")
            null
            (list (insert 0 (string-length text) "\n" #f))))
      (cons inserts (loop (cdr texts) (cdr kinds) e))]
     [else
      (define text (car texts))
      (define kind (car kinds))
      (define-values (new-e repl output)
        (cond
         [(or (not (keyword? kind))
              (regexp-match? #px"^\\s*#lang" text))
          ;; possibly make a new evaluator; no repl content to eval
          (define new-e
            (cond
             [(or (not e) (keyword? kind))
              (when e (kill-evaluator e))
              (define-values (evals pre-output)
                (do-eval #f (lambda ()
                              (make-mail-evaluator declared-modules 'racket/base))))
              (and (not (void? evals))
                   (car evals))]
             [else e]))
          (define module-name
            (cond
             [(keyword? kind) 'program]
             [(symbol? kind)
              (hash-set! declared-modules kind kind)
              kind]
             [else (path->complete-path kind)]))
          (define quoted-module-name
            (cond
             [(symbol? module-name) `',module-name]
             [else module-name]))
          (define-values (ignored output)
            (if new-e
                (do-eval
                 new-e
                 (lambda ()
                   (call-in-sandbox-context
                    new-e
                    (lambda ()
                      (define p (open-input-string text))
                      (port-count-lines! p)
                      (define form
                        (with-module-reading-parameterization
                            (lambda () (read-syntax 'program p))))
                      (unless (eof-object? form)
                        (check-module-form form 'module #f)
                        (unless (eof-object? (read p))
                          (error "found extra after module"))
                        (parameterize ([current-module-declare-name
                                        (make-resolved-module-path module-name)])
                          (eval form))
                        (namespace-require quoted-module-name)
                        (current-namespace (module->namespace quoted-module-name)))))))
                (values #f #f)))
          (values new-e
                  #f
                  output)]
         [(not e)
          ;; create a default evaluator
          (values
           (make-mail-evaluator declared-modules 'racket)
           text
           #f)]
         [else
          ;; continue using the evaluator
          (values e text #f)]))
      ;; When we have a REPL-style ``` block, look for prompts
      ;; and evaluate expressions after them:
      (cond
       [repl
        (define i (open-input-string text))
        (port-count-lines! i)
        (define inserts
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
                                out
                                #f)
                        (loop))])])]
             [m
              ;; Found an empty prompt; skip it
              (loop)]
             [else
              ;; No more prompts
              null])))
        (cons inserts (loop (cdr texts) (cdr kinds) new-e))]
       [(and output
             (positive? (send output last-position)))
        (indent-output output 0)
        (cond
         [(and (pair? (cdr kinds))
               (eq? '#:output (cadr kinds)))
          ;; No changes to module, but update output block:
          (cons null
                (cons (list
                       (insert 0 (string-length (cadr texts)) output #f))
                      (loop (cddr texts)
                            (cddr kinds)
                            new-e)))]
         [else
          ;; Insert output block:
          (define end (string-length (car texts)))
          (define prefix (if (and (positive? end)
                                  (equal? #\newline (string-ref (car texts) (sub1 end))))
                             ""
                             "\n"))
          (cons (list
                 (insert end end (string-append prefix "```\nOutput:\n```") #t)
                 (insert end end output #t))
                (loop (cdr texts) (cdr kinds) new-e))])]
       [else
        (cons null (loop (cdr texts) (cdr kinds) new-e))])])))

;; ----------------------------------------
;; Evaluation helpers

(define (make-mail-evaluator declared-modules lang)
  (call-with-sandbox-config
   (lambda ()
     (define e (make-evaluator lang))
     (install-printer e)
     (install-module-name-resolver declared-modules e)
     e)))

(define (call-with-sandbox-config thunk)
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-error-output current-output-port]
                    [sandbox-propagate-breaks #f]
                    [sandbox-make-code-inspector make-inspector]
                    [sandbox-namespace-specs (append (sandbox-namespace-specs)
                                                     (list 'pict))])
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
     (define vs
       (parameterize ([sandbox-output o])
         (call-with-values thunk list)))
     (when e
       (call-in-sandbox-context e (lambda ()
                                    (map (current-print) vs))))
     vs)
   editor))

(define (file-char-position i)
  (define-values (line col pos) (port-next-location i))
  (sub1 pos))

;; ----------------------------------------
;; Pretty printing

(define (install-printer e)
  (call-in-sandbox-context
   e
   (lambda ()
     (current-print
      (dynamic-require 'racket/pretty 'pretty-print-handler))
     ((dynamic-require 'racket/pretty 'pretty-print-size-hook)
      (lambda (v display? p)
        (if (and (port-writes-special? p)
                 (or (v . is-a? . snip%)
                     (pict? v)))
            1
            #f)))
     ((dynamic-require 'racket/pretty 'pretty-print-print-hook)
      (lambda (v display? p)
        (define new-v
          (cond
           [(pict? v) (new pict-snip% [pict v])]
           [else v]))
        (write-special new-v p))))))

;; ----------------------------------------
;; Finding declared modules

(define (install-module-name-resolver declared-modules e)
  (call-in-sandbox-context
   e
   (lambda ()
     (current-module-name-resolver
      (let ([r (current-module-name-resolver)])
        (case-lambda
          [(m ns) (r m ns)]
          [(mp wrt stx load?)
           (cond
            [(hash-ref declared-modules mp #f)
             (make-resolved-module-path mp)]
            [(and (pair? mp)
                  (eq? 'submod (car mp))
                  (hash-ref declared-modules (cadr mp) #f))
             (make-resolved-module-path (cdr mp))]
            [else
             (r mp wrt stx load?)])]))))))

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

;; ----------------------------------------
;; Allowing evaluator GUI events and cleaning up evaluators

(define (pump-evaluator e)
  ;; Not a pretty way to manage a sandbox, but good enough for
  ;; our purposes:
  (thread (lambda ()
            (let loop ()
              (when (null? (get-top-level-windows))
                (kill-evaluator e))
              (when (evaluator-alive? e)
                (sync
                 (thread
                  (lambda ()
                    (call-in-sandbox-context
                     e
                     (lambda () (sleep/yield 0.5))))))
                (loop))))))
