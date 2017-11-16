(use-modules (web server)
             (web response)
             (web request)
             (sxml simple)
             (json)
             (ice-9 match)
             (ice-9 regex)
             (ice-9 hash-table)
             (srfi srfi-1)
             (rnrs bytevectors)
             (web uri))

(load "lib/disks.scm")
(load "lib/tz.scm")

(define sym 'access-control-allow-origin)

(define installer-save-file
  (or (getenv "INSTALLER_SAVE_FILE") "installer.json"))
(define installer-conf-file
  (or (getenv "INSTALLER_CONF_FILE") "configuration.nix"))
(define nixos-manual
  (getenv "NIXOS_MANUAL"))

(define %mime-types
  (alist->hash-table
    '(("css" . text/css)
      ("gif" . image/gif)
      ("html" . text/html)
      ("svg" . image/svg+xml)
      ("js" . application/javascript)
      ("json" . text/javascript))))

(define %file-ext-regexp
  (make-regexp "(\\.(.*)|[~%])$"))

(define (file-extension file-name)
  "Return the file extension for FILE-NAME, or #f if one is not found."
  (and=> (regexp-exec %file-ext-regexp file-name)
         (lambda (match)
           (or (match:substring match 2)
               (match:substring match 1)))))

(define (mime-type file-name)
  "Guess the MIME type for FILE-NAME based upon its file extension."
  (or (hash-ref %mime-types (file-extension file-name))
      'text/plain))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define* (send-text data #:key (code 200))
  (values (build-response #:code code
                          #:headers `((access-control-allow-origin . "*")
                                      (content-type . (text/plain))))
          (lambda (port)
            (display data port))))

(define* (send-json data #:key (code 200))
  (values (build-response #:code code
                          #:headers `((access-control-allow-origin . "*")
                                      (content-type . (application/json))))
          (lambda (port)
            (scm->json data port))))


(define* (send-file path #:key (code 200) (mime (mime-type path)))
         (if (file-exists? path)
           (values (build-response
                     #:code code
                     #:headers `((access-control-allow-origin . "*")
                                 (content-type mime)))
                   (lambda (port)
                     (display (read-file path) port)))
           (values (build-response #:code 404) "resource not found")))


(define* (serve-manual request)
  (let ((path (string-join
                (cons nixos-manual (delete ".." (request-path-components request)))
                file-name-separator-string)))
    (display path) (newline)
    (send-file path)))

(define* (serve-assets request)
  (let ((path (string-join
                (cons "ui" (delete ".." (request-path-components request)))
                file-name-separator-string)))
    (display path) (newline)
    (send-file path)))

(define disks (detect-disks))

(define detected-disks (detect-disks))
(define detected-timezones (timezones))

(define (read-file path)
  (let* ((port (open-input-file path))
         (body (get-string-all port)))
    (close-input-port port)
    body))

(define (write-file path data)
  (let* ((port (open-output-file path)))
    (display data port)
    (close-output-port port)))

(define (installer-handler request request-body)
  (cond ((eq? (request-method request) 'GET)
         (match (request-path-components request)
           (()
            (send-file "ui/index.html"))
           (("nixos" _ ...)
            (serve-manual request))
           (("assets" _ ...)
            (serve-assets request))
           (("index.js")
            (send-file "ui/index.js"))
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
            (display (utf8->string request-body))
            (newline)
            (write-file installer-save-file (utf8->string request-body))
            (write-file installer-conf-file (read-file "template/configuration.nix"))
            (write-file "/mnt/etc/nixos/ssh.nix" (read-file "template/ssh.nix"))
            (send-text "saved"))))))

(run-server installer-handler 'http '(#:port 8081))
