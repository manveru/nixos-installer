(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define-class <timezone> ()
  (tz-country #:accessor tz-country
               #:init-keyword #:tz-country)
  (tz-coords #:accessor tz-coords
                #:init-keyword #:tz-coords)
  (tz-region #:accessor tz-region
               #:init-keyword #:tz-region)
  (tz-city #:accessor tz-city
               #:init-keyword #:tz-city)
  (tz-name #:accessor tz-name
                 #:init-keyword #:tz-name)
  (tz-comment #:accessor tz-comment
                 #:init-keyword #:tz-comment))

(define (format-city region-city)
  (regexp-substitute/global #f "_" (cadr region-city) 'pre " " 'post))

(define (timezones)
  (let* ((zonetab
           (open-input-file
             (string-append (getenv "TZDIR") "/zone.tab")))
         (zonetab-content
           (get-string-all zonetab))
         (all-lines (string-split zonetab-content #\newline))
         (lines (filter
                  (lambda (line)
                    (and
                      (> (string-length line) 0)
                      (not (string=? "#" (substring line 0 1)))))
                    all-lines)))
    (close-input-port zonetab)
    (map (lambda (line)
           (let* ((parts (string-split line #\tab))
                  (region-city (string-split (caddr parts) #\/))
                  (comment (cdddr parts)))
             (make <timezone>
                 #:tz-country (car parts)
                 #:tz-coords (cadr parts)
                 #:tz-name (caddr parts)
                 #:tz-comment (if (nil? comment) "" (car comment))
                 #:tz-region (car region-city)
                 #:tz-city (format-city region-city))))
         lines)))

(define (timezones->json zones)
  (define (timezone->json zone)
    (list (cons "country" (tz-country zone))
          (cons "coords" (tz-coords zone))
          (cons "region" (tz-region zone))
          (cons "city" (tz-city zone))
          (cons "name" (tz-name zone))
          (cons "comment" (tz-comment zone))))
  (map timezone->json zones))
