#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/nsstring)

(provide set-app-badge!)

(import-class NSDockTile
              NSApplication)

(define (set-app-badge! new-mail?)
  (when (eq? 'macosx (system-type))
    (define tile (tell (tell NSApplication sharedApplication) dockTile))
    (if new-mail?
        (tell tile setBadgeLabel: #:type _NSString "!")
        (tell tile setBadgeLabel: #f))))
