#lang racket/base
(require racket/class
         "oauth2/main.rkt")

(provide oauth2-get-access-token)

(define (oauth2-get-access-token auth-url token-url client-id)
  (define the-oauth2
    (oauth2/request-auth-code/browser
     (oauth2-auth-server #:auth-url auth-url
                         #:token-url token-url)
     (oauth2-client #:id (or client-id
                             ;; SirMail client ID registered with Microsoft:
                             "75e35857-2e10-4059-87c0-cd4e067dcb7f"))
     '("profile"
       "email"
       "openid"
       "offline_access"
       "https://outlook.office.com/IMAP.AccessAsUser.All"
       "https://outlook.office.com/POP.AccessAsUser.All"
       "https://outlook.office.com/SMTP.Send offline_access")
     #:servlet-path #f
     #:ssl-cert "/Users/mflatt/handin/server/server-cert.pem"
     #:ssl-key "/Users/mflatt/handin/server/private-key.pem"))
  (send the-oauth2 get-access-token #:re-acquire? #t))
