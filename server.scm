(use-modules (web server)
             (web response)
             (sxml simple))

(load "lib/template.scm")
(load "lib/disks.scm")

(define* (send-xml msg #:key (code 200))
  (define doctype "<!DOCTYPE html>\n")
  (values (build-response #:code code
                          #:headers `((content-type . (text/html))))
          (lambda (port)
            (begin (display doctype port)
                   (sxml->xml msg port)))))

(define disks (detect-disks))

(define (hello-world-handler request request-body)
  (send-xml (index-page disks)))

(run-server hello-world-handler 'http '(#:port 8081))
