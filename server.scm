(use-modules (web server)
             (web response)
             (web request)
             (sxml simple)
             (json)
             (ice-9 match)
             (web uri))

(load "lib/template.scm")
(load "lib/disks.scm")
(load "lib/tz.scm")

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

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

(define (installer-handler request request-body)
  (match (request-path-components request)
    (()
     (send-xml (index-page disks)))
    (("timezones")
     (send-json (timezones->json (timezones))))
    (("disks")
     (send-json (disks->json (detect-disks))))))

(run-server installer-handler 'http '(#:port 8081))
