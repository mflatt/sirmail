(module spell mzscheme
  (require (lib "lex.ss" "parser-tools")
           (prefix : (lib "lex-sre.ss" "parser-tools"))
           (lib "class.ss")
           (lib "string.ss")
           (lib "list.ss")
           (lib "framework.ss" "framework")
           (lib "contract.ss")
	   (lib "file.ss")
           (lib "process.ss"))
  
  (provide/contract [activate-spelling ((is-a?/c color:text<%>) . -> . void?)]
                    [word-count (-> number?)])
  
  (define-lex-abbrevs
   (extended-alphabetic (:or alphabetic #\'))
   (word (:: (:* extended-alphabetic) (:+ alphabetic) (:* extended-alphabetic)))
   (paren (char-set "()[]{}")))
                                   
  (define get-word
    (lexer 
     ((:+ whitespace)
      (values lexeme 'white-space #f (position-offset start-pos) (position-offset end-pos)))
     (paren
      (values lexeme 'other (string->symbol lexeme) (position-offset start-pos) (position-offset end-pos)))
     ((:+ (:~ (:or alphabetic whitespace paren)))
      (values lexeme 'other #f (position-offset start-pos) (position-offset end-pos)))
     (word
      (let ((ok (spelled-correctly? lexeme)))
        (values lexeme (if ok 'other 'error) #f (position-offset start-pos) (position-offset end-pos))))
     ((eof)
      (values lexeme 'eof #f #f #f))))
  
  (define (activate-spelling t)
    (send t start-colorer
          (lambda (s) (format "framework:syntax-coloring:scheme:~a" s))
          get-word
          `((|(| |)|)
            (|[| |]|)
            (|{| |}|))))

  (define ask-chan (make-channel))
  (define word-count-chan (make-channel))
  
  ;; spelled-correctly? : string -> boolean
  (define (spelled-correctly? word)
    (sync
     (nack-guard-evt
      (lambda (failed)
        (let ([result (make-channel)])
          (channel-put ask-chan (list result failed word))
          result)))))
  
  (define (word-count) 
    (let ([c (make-channel)])
      (channel-put word-count-chan c)
      (channel-get c)))
  
  (thread
   (lambda ()
     (let ([bad-dict (make-hash-table 'equal)])
       (let loop ([computed? #f]
                  [dict #f])
         (sync
          (handle-evt
           ask-chan
           (lambda (lst)
             (let-values ([(answer-chan give-up-chan word) (apply values lst)])
               (let ([computed-dict (if computed?
                                        dict
                                        (fetch-dictionary))])
                 (sync
                  (handle-evt
                   (channel-put-evt answer-chan (check-word computed-dict bad-dict word))
                   (lambda (done)
                     (loop #t computed-dict)))
                  (handle-evt
                   give-up-chan
                   (lambda (done)
                     (loop #t computed-dict))))))))
          (handle-evt
           word-count-chan
           (lambda (ans)
             (let ([count (if dict (hash-table-count dict) 0)])
               (thread (lambda () (channel-put ans count))))
             (loop computed? dict))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; The code below all runs in the above thread (only)
  ;;;
  
  (define extra-words '("sirmail" "mred" "drscheme" "mzscheme" "plt"))
  
  (define has-ispell? 'dontknow)
  (define ispell-prog #f)
  (define ispell-in #f)
  (define ispell-out #f)
  (define ispell-err #f)
  (define (ispell-word word)
    (when (eq? has-ispell? 'dontknow)
      (let ([existing (or (find-executable-path (if (eq? (system-type) 'windows)
						    "ispell.exe"
						    "ispell")
						#f)
			  (ormap (lambda (x) (and (file-exists? x) x))
				 '("/sw/bin/ispell"
				   "/usr/bin/ispell"
				   "/bin/ispell"
				   "/usr/local/bin/ispell")))])
        (if (not existing)
            (set! has-ispell? #f)
            (begin
              (set! has-ispell? #t)
              (set! ispell-prog existing)))))
    (cond
      [has-ispell?
       (unless (and ispell-in ispell-out ispell-err)
         (let-values ([(out in pid err status) (apply values (process* ispell-prog 
								       "-a"
								       "-w"
								       ;; Tell ispell to treat every character
								       ;;  as part of a word, because our lexer
								       ;;  has already separated words.
								       (let loop ([n 255][l null])
									 (if (= n 0)
									     (apply string-append l)
									     (loop (sub1 n)
										   (cons
										    (format "n~a" n)
										    l))))))])
           (let ([version-line (read-line out)])
             (debug "< ~s\n" version-line))
           
           (set! ispell-in in)
           (set! ispell-out out)
           (set! ispell-err err)))
       
       (let ([to-send (format "^~a\n" word)])
         (debug "> ~s\n" to-send)
         (display to-send ispell-in)
	 (flush-output ispell-in))

       (let* ([answer-line (read-line ispell-out 'any)]
              [_ (debug "< ~s\n" answer-line)]
              [blank-line (read-line ispell-out 'any)]
              [_ (debug "< ~s\n" blank-line)])
         (unless (equal? blank-line "")
           (fprintf (current-error-port) "expected blank line from ispell, got (word ~s):\n~a\nrestarting ispell\n\n" 
                    word
                    blank-line)
           (close-output-port ispell-in)
           (close-input-port ispell-out)
           (close-input-port ispell-err)
           (set! ispell-in #f)
           (set! ispell-out #f)
           (set! ispell-err #f))
         (not (not (regexp-match #rx"^[\\+\\-\\*]" answer-line))))]
      [else #f]))

  (define (debug str . args)
    (when (getenv "PLTISPELL")
      (apply printf str args)))
  
  ;; fetch-dictionary : -> (union #f hash-table)
  ;; computes a dictionary, if any of the possible-file-names exist
  ;; for now, just return an empty table. Always use ispell
  (define (fetch-dictionary) (make-hash-table 'equal))
  
  (define (fetch-dictionary/not-used)
    (let* ([possible-file-names '("/usr/share/dict/words"
                                  "/usr/share/dict/connectives"
                                  "/usr/share/dict/propernames"
                                  "/usr/dict/words")]
           [good-file-names (filter file-exists? possible-file-names)])
      (and (not (null? good-file-names))
	   (let ([d (make-hash-table 'equal)])
	     (for-each (lambda (word) (hash-table-put! d word #t)) extra-words)
	     (for-each 
	      (lambda (good-file-name)
		(call-with-input-file* good-file-name
				       (lambda (i)
					 (let loop ()
					   (let ((word (read-line i)))
					     (unless (eof-object? word)
						     (hash-table-put! d word #t)
						     (loop)))))))
	      good-file-names)
	     d))))
  
  ;; check-word : hash-table hash-table string -> boolean
  (define (check-word dict bad-dict word)
    (let* ([word-ok (lambda (w) (hash-table-get dict w (lambda () #f)))]
           [word-bad (lambda (w) (hash-table-get bad-dict w (lambda () #f)))]
           [subword-ok (lambda (reg)
                         (let ([m (regexp-match reg word)])
                           (and m
                                (word-ok (cadr m)))))])
      (if dict
          (cond
            [(word-ok word) #t]
            [(word-ok (string-lowercase! (string-copy word))) #t]
            [(word-bad word) #f]
            [else
             (let ([ispell-ok (ispell-word word)])
               (if ispell-ok 
                   (hash-table-put! dict word #t)
                   (hash-table-put! bad-dict word #t))
                ispell-ok)])
          #t))))
