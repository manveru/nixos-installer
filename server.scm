(use-modules (web server))
(use-modules (web response))

(load "lib/template.scm")

(define* (send-xml msg #:key (code 200))
  (define doctype "<!DOCTYPE html>\n")
  (values (build-response #:code code
                          #:headers `((content-type . (text/html))))
          (lambda (port)
            (begin (display doctype port)
                   (sxml->xml msg port)))))

(define (hello-world-handler request request-body)
  (send-xml (index-page '())))

(run-server hello-world-handler 'http '(#:port 8081))
