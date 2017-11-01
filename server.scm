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

(define sym 'access-control-allow-origin)

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define* (send-text data #:key (code 200))
  (values (build-response #:code code
                          #:headers `((access-control-allow-origin . "*")
                                      (content-type . (text/plain))))
          (lambda (port)
            (display data port))))

(define* (send-xml msg #:key (code 200))
  (define doctype "<!DOCTYPE html>\n")
  (values (build-response #:code code
                          #:headers `((access-control-allow-origin . "*")
                                      (content-type . (text/html))))
          (lambda (port)
            (begin (display doctype port)
                   (sxml->xml msg port)))))

(define* (send-json data #:key (code 200))
  (values (build-response #:code code
                          #:headers `((access-control-allow-origin . "*")
                                      (content-type . (application/json))))
          (lambda (port)
            (scm->json data port))))

(define* (send-html data #:key (code 200))
  (values (build-response #:code code
                          #:headers `((access-control-allow-origin . "*")
                                      (content-type . (text/html))))
          (lambda (port)
            (display data port))))

(define* (send-svg data #:key (code 200))
  (values (build-response #:code code
                          #:headers `((content-type . (image/svg+xml))))
          (lambda (port)
            (display data port))))

(define disks (detect-disks))

(define detected-disks (detect-disks))
(define detected-timezones (timezones))
(define (read-file path)
  (let* ((port (open-input-file path))
         (body (get-string-all port)))
    (close-input-port port)
    body))

(define (installer-handler request request-body)
  (cond ((eq? (request-method request) 'GET)
         (match (request-path-components request)
           (()
            (send-html (read-file  "ui/index.html")))
           (("logo.svg")
            (send-svg (read-file "ui/logo.svg")))
           (("timezones")
            (send-json (timezones->json detected-timezones)))
           (("disks")
            (send-json (disks->json detected-disks)))
           (failed
            (values (build-response #:code 404) "resource not found"))))
        ((eq? (request-method request) 'OPTIONS)
         (send-text "ok"))
        ((eq? (request-method request) 'POST)
         (match (request-path-components request)
           (("save")
            (display (jq (utf8->string request-body) "."))
            (newline)
            (let ((port (open-output-file "out.json")))
                   (display (jq (utf8->string request-body) ".") port)
                   (close-output-port port))
            (send-text "saved"))))))

(run-server installer-handler 'http '(#:port 8081))
