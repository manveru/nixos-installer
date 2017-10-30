(use-modules (oop goops)
             (ice-9 popen)
             (srfi srfi-13) ;; strings
             (srfi srfi-1)
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

(define (filter-out-devices-without-vendor json)
  (jq json "[ .blockdevices[] | select( .vendor != null) ]"))

(define (make-devices-from-json json)
  (define (make-device-from-json-index index)
    (let ((machine-json (jq json (string-concatenate (list ".["
                                                           (number->string index)
                                                           "]")))))
      (make <disk>
        #:device-path (jq machine-json ".name")
        #:device-model (jq machine-json ".model")
        #:device-size (jq machine-json ".size")
        #:device-serial (jq machine-json ".serial"))))
  (let* ((number-of-devices (string->number (jq json "length")))
         (indeces (iota number-of-devices)))
    (map make-device-from-json-index indeces)))

(define (detect-disks-from-json json-string)
  (let ((filtered-devices (filter-out-devices-without-vendor json-string)))
    (make-devices-from-json filtered-devices)))
