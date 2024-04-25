;; Derived from the `webapi` package:
;; Copyright 2011-2012 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/string
         net/url
         web-server/http
         web-server/http/bindings
         web-server/templates
         "net.rkt"
         "oauth2.rkt"
         "../wkwebview.rkt"
         web-server/servlet-env)
(provide oauth2/request-auth-code/browser)

(define local-browser? (and #t (eq? 'macosx (system-type))))

(define (oauth2/request-auth-code/browser auth-server client scopes
                                          #:port [port 8000]
                                          #:servlet-path [servlet-path "/oauth2"]
                                          #:ssl-cert [ssl-cert #f]
                                          #:ssl-key [ssl-key #f])
  (let ([oauth2 (new oauth2%
                     (auth-server auth-server)
                     (client client))])
    (let ([auth-code (request-auth-code/web oauth2 scopes
                                            #:port port
                                            #:servlet-path servlet-path
                                            #:ssl-cert ssl-cert
                                            #:ssl-key ssl-key)])
      (send oauth2 acquire-token/auth-code!
            auth-code
            #:redirect-uri (format "http~a://localhost:~a~a"
                                   (if ssl-cert "s" "")
                                   port
                                   (if servlet-path
                                       (format "~a/response" servlet-path)
                                       ""))
            #:who 'oauth2/request-access/web)
      oauth2)))

(define (request-auth-code/web oauth2 scopes
                               #:who [who 'request-access/web]
                               #:port port
                               #:servlet-path servlet-path
                               #:ssl-cert ssl-cert
                               #:ssl-key ssl-key)
  (let ([chan (make-channel)]
        [server-cust (make-custodian)])
    (define start-path (format "~a/init" (or servlet-path "")))
    (parameterize ((current-custodian server-cust))
      (thread
       (lambda ()
         (serve/servlet (make-servlet oauth2 scopes chan port servlet-path ssl-cert)
                        #:launch-browser? (not local-browser?)
                        #:quit? #t
                        #:banner? #f
                        #:port port
                        #:ssl? #f
                        #:ssl-cert ssl-cert
                        #:ssl-key ssl-key
                        #:servlet-path start-path
                        #:servlet-regexp #rx"^"
                        #:extra-files-paths null))))
    (define close-browser
      (if local-browser?
          (run-browser (format "https://localhost:~a~a" port start-path))
          void))
    (begin0
      (channel-get chan)
      (close-browser)
      (custodian-shutdown-all server-cust))))

(define (make-servlet oauth2 scopes chan port servlet-path ssl-cert)
  (lambda (req)
    (let ([path (string-join (map path/param-path (url-path (request-uri req))) "/")])
      (cond [(regexp-match? #rx"init$" path)
             (redirect-to
              (let ([auth-server (send oauth2 get-auth-server)]
                    [client (send oauth2 get-client)])
                (send auth-server get-auth-request-url
                      #:client client
                      #:scopes scopes
                      #:redirect-uri (format "http~a://localhost:~a~a"
                                             (if ssl-cert "s" "")
                                             port
                                             (if servlet-path
                                                 (format "~a/response" servlet-path)
                                                 "")))))]
            [(if servlet-path
                 (regexp-match? #rx"response$" path)
                 #true)
             (let ([bindings (request-bindings/raw req)])
               (cond [(bindings-assq #"code" bindings)
                      => (lambda (code-b)
                           (channel-put chan
                                        (bytes->string/utf-8
                                         (binding:form-value code-b)))
                           (response/full
                            200 #"Okay"
                            (current-seconds) TEXT/HTML-MIME-TYPE
                            null
                            (list (string->bytes/utf-8
                                   (include-template "static-got-auth-code.html")))))]
                     [(bindings-assq #"error" bindings)
                      => (lambda (err-b)
                           (error "Failed: ~s" (binding:form-value err-b)))]
                     [else (error "Bad response!")]))]
            [else (error "Invalid URL: ~s" path)]))))
