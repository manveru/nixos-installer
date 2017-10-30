(use-modules (sxml simple))
(use-modules (srfi srfi-1))

(define username-form
  `(div (p "username:")
        (form (input (@ (type "text"))))))

(define (index-page devices)
  (define html-head
    `(head (title "NixOS installer")))
  (define html-body
    `(body (h1 "NixOS installer")
           ,username-form))
    `(html ,html-head ,html-body)
  )
