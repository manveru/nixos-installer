(use-modules (web server)
             (web response)
             (web request)
             (sxml simple)
             (json)
             (ice-9 match)
             (rnrs bytevectors)
             (web uri))

(load "lib/template.scm")
(load "lib/disks.scm")
(load "lib/tz.scm")

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (output-post-data msg)
  (display (utf8->string msg))
  (newline))

(define* (send-xml msg #:key (code 200))
  (define doctype "<!DOCTYPE html>\n")
  (values (build-response #:code code
                          #:headers `((content-type . (text/html))))
          (lambda (port)
            (begin (display doctype port)
                   (sxml->xml msg port)))))

(define* (send-json data #:key (code 200))
  (values (build-response #:code code
                          #:headers `((content-type . (application/json))))
          (lambda (port)
            (scm->json data port))))

(define disks (detect-disks))

(define detected-disks (detect-disks))
(define detected-timezones (timezones))

(define (installer-handler request request-body)
  (cond ((eq? (request-method request) 'GET)
         (match (request-path-components request)
           (()
            (send-xml (index-page disks)))
           (("timezones")
            (send-json (timezones->json detected-timezones)))
           (("disks")
            (send-json (disks->json detected-disks)))
           (failed
            (values (build-response #:code 404) "resource not found"))))
        ((eq? (request-method request) 'POST)
         (begin
           (output-post-data request-body)
           (values (build-response #:code 200) "ok")))))

(run-server installer-handler 'http '(#:port 8081))
