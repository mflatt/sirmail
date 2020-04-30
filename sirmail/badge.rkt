#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/nsstring)

(provide add-badge!
         remove-badge!)

(import-class NSDockTile
              NSApplication)

(define badged (make-hasheq))

(define (add-badge! id)
  (hash-set! badged id #t)
  (update-badge!))

(define (remove-badge! id)
  (hash-remove! badged id)
  (update-badge!))

(define (update-badge!)
  (when (eq? 'macosx (system-type))
    (define tile (tell (tell NSApplication sharedApplication) dockTile))
    (if (positive? (hash-count badged))
        (tell tile setBadgeLabel: #:type _NSString "(+)")
        (tell tile setBadgeLabel: #f))))
