(use-modules (oop goops)
             (ice-9 popen)
             (srfi srfi-13) ;; strings
             (ice-9 textual-ports)
             )

(load "jq.scm")

(define-class <disk> ()
  (device-path #:accessor device-path
               #:init-keyword #:device-path)
  (device-model #:accessor device-model
                #:init-keyword #:device-model)
  (device-size #:accessor device-size
               #:init-keyword #:device-size)
  (device-serial #:accessor device-serial
                 #:init-keyword #:device-serial))

(define (detect-disks-from-json json-string)
  '())
