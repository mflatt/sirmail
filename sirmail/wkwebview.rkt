;; Based on https://github.com/soapdog/racket-web-view
;; under MIT license

#lang racket/base
(require framework
         ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/nsstring
         ffi/unsafe/nsalloc
         racket/gui/base
         racket/class)

(provide wk-web-view%
         run-browser)

(void (ffi-lib "/System/Library/Frameworks/WebKit.framework/WebKit"))
(import-class WKWebView)
(import-class WKWebViewConfiguration)
(import-class NSURLRequest)
(import-class NSURL)
(import-class NSView)
(import-class NSString)
(import-class NSObject)
(import-class NSURLCredential)
(import-protocol WKNavigationDelegate)

(define _NSInteger _int)
(define NSURLSessionAuthChallengeUseCredential 0)

;; We allocate one frame in its' own eventspace, so we
;; don't have to worry about freeing any non-local
;; WebKit resources
(define browser-eventspace (make-eventspace))
(define browser-frame #f)
(define browser-wv #f)

(define (start-browser!)
  (unless browser-frame
    (define ch (make-channel))
    (parameterize ([current-eventspace browser-eventspace])
      (queue-callback
       (lambda ()
         (set! browser-frame (new frame%
                                  [label "Login"]
                                  [width 800]
                                  [height 800]))
         (set! browser-wv
               (new wk-web-view%
                    [parent browser-frame]))
         (channel-put ch 'ready))))
    (channel-get ch)))

(define (run-browser url)
  (start-browser!)
  (define ch (make-channel))
  (parameterize ([current-eventspace browser-eventspace])
    (queue-callback
     (lambda ()
       (void (send browser-wv set-url url))
       (send browser-frame show #t)
       (channel-put ch 'done))))
  (channel-get ch)
  (lambda ()
    (parameterize ([current-eventspace browser-eventspace])
      (queue-callback
       (lambda ()
         (send browser-frame show #f))))))

(define wk-web-view%
  (class object%
    (super-new)

    (init parent [on-status-change #f])

    (define/private (parent-on-size w h)
      (on-size w h))
    (define canvas
      (new (class canvas%
             (super-new)
             (define/override (on-size w h)
               (parent-on-size w h)))
           [parent parent]))
    (send canvas show #f)

    (define-cstruct _NSPoint ([x _double*]
                              [y _double*]))

    (define-cstruct _NSSize ([width _double*]
                             [height _double*]))
        
    (define-cstruct _CGRect ([origin _NSPoint]
                             [size _NSSize]))

    (define webview-x (send parent get-x))

    (define webview-y (send parent get-y))

    (define-values (webview-width webview-height)
      (send parent get-client-size))
    
    (define configuration 
      (tell (tell WKWebViewConfiguration alloc) init))

    (define (release id-ptr) (tell id-ptr release))

    (define (make-rect)
      (make-CGRect (make-NSPoint webview-x webview-y) (make-NSSize webview-width webview-height)))

    (define webview
      (with-autorelease
       (tell (tell (tell WKWebView alloc) 
                   initWithFrame: #:type _CGRect (make-rect)
                   configuration: configuration)
             retain)))
  
    (define-objc-class WebViewDelegate NSObject
      #:protocols (WKNavigationDelegate)
      []
      (-a _void (webView: [_id view]
                          didFailNavigation: [_id navigation]
                          withError: [_id error])
          (log-error "error"))
      (-a _void (webView: [_id view]
                          didReceiveAuthenticationChallenge: ch
                          completionHandler: finish)
          (with-autorelease
           (define credential (tell (tell NSURLCredential alloc) initWithTrust: (tell (tell ch protectionSpace) serverTrust)))
           ((cast (objc-block-function-pointer finish)
                  _fpointer
                  (_fun _id _NSInteger _id -> _void))
            finish
            NSURLSessionAuthChallengeUseCredential
            credential)
           (void)))
      (-a _void (webView: [_id view] didCommitNavigation: [_id navigation])
          (if on-status-change (on-status-change "loading...") #f))
      (-a _void (webView: [_id view] didFinishNavigation: [_id navigation])
          (if on-status-change (on-status-change "page loaded") #f)))

    (define delegate
      (with-autorelease
       (tell (tell (tell WebViewDelegate alloc) init) retain)))

    (with-autorelease
     (tell (send parent get-client-handle) addSubview: webview)
     (tellv webview setNavigationDelegate: delegate))

    (define/public (get-title)
      (with-autorelease
       (tell #:type _NSString webview title)))

    (define/public (get-url)
      (with-autorelease
       (define url (tell webview URL))
       (tell #:type _NSString url absoluteString)))

    (define/public (set-url given-url)
      (with-autorelease
       (let* ([url-string (tell (tell NSString alloc)
                                initWithUTF8String: #:type _string given-url)]
              [url (tell NSURL URLWithString: url-string)]
              [req (tell NSURLRequest requestWithURL: url)])
         (tell webview loadRequest: req))))

    (define/public (can-handle-url? given-url)
      (with-autorelease
       (let* ([url-string (tell (tell NSString alloc)
                                initWithUTF8String: #:type _string given-url)])
         (define ret (tell webview handlesURLScheme: url-string ))
         (release url-string)
         ret)))

    (define/public (go-forward)
      (with-autorelease
       (tell webview goForward)))

    (define/public (go-back)
      (with-autorelease
       (tell webview goBack)))

    (define/public (reload)
      (with-autorelease
       (tell webview reload)))

    (define/public (set-html-text text base-url)
      (with-autorelease
       (let* ([url-string (tell (tell NSString alloc)
                                initWithUTF8String: #:type _string base-url)]
              [url (tell NSURL URLWithString: url-string)]
              [html (tell (tell NSString alloc)
                          initWithUTF8String: #:type _string text)])
         (tell webview loadHTMLString: html baseURL: url))))

    (define/public (on-size w h)
      (with-autorelease
       (tellv webview setFrame:
              #:type _CGRect (make-CGRect (make-NSPoint 0 0) 
                                          (make-NSSize w h)))))))

(module+ main
  (define close (run-browser "https://racket-lang.org/"))
  (void (yield browser-eventspace))
  (close))
