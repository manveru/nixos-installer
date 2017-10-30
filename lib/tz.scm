(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

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
           (zip
             '("country" "coords" "name" "comment")
             (string-split line #\tab)))
         lines)))
