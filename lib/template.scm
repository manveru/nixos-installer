(use-modules (sxml simple)
             (srfi srfi-1))

(load "tz.scm")

(define input-username
  `(div (p "username:")
        (input (@ (type "text")))))

(define input-timezone
  `(select (@ (name "timezone"))
           ,@(map (lambda (name)
                    `(option (@ (value ,name)) ,name))
                  (timezone-names))))

(define form
  `(form ,input-username ,input-timezone))

(define (index-page devices)
  (define html-head
    `(head (title "NixOS installer")))
  (define html-body
    `(body (h1 "NixOS installer")
           ,form))
    `(html ,html-head ,html-body)
  )
