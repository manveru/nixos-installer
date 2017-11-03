(use-modules (oop goops)
             (ice-9 popen)
             (srfi srfi-13) ;; strings
             (srfi srfi-1)
             (ice-9 textual-ports)
             (json)
             )

(load "io.scm")

(define-class <disk> ()
  (device-path #:accessor device-path
               #:init-keyword #:device-path)
  (device-model #:accessor device-model
                #:init-keyword #:device-model)
  (device-size #:accessor device-size
               #:init-keyword #:device-size)
  (device-serial #:accessor device-serial
                 #:init-keyword #:device-serial))

(define (make-devices-from-json json)
  (define (make-device-from-json-index json-device)
    (make <disk>
      #:device-path (hash-ref json-device "name")
      #:device-model (hash-ref json-device "model")
      #:device-size (hash-ref json-device "size")
      #:device-serial (hash-ref json-device "serial")))
  (let* ((blockdevices (hash-ref json "blockdevices"))
         (number-of-devices (length blockdevices)))
    (map make-device-from-json-index
         (filter (lambda (device)
                   (hash-ref device "serial"))
                 blockdevices))))

(define (detect-disks-from-json json-string)
  (make-devices-from-json (json-string->scm json-string)))

(define (detect-disks)
  (let ((lsblk-output (read-process-string "lsblk" "-O" "--json")))
    (detect-disks-from-json lsblk-output)))

(define (disks->json disks)
  (define (disk->json disk)
    (list (cons "path" (device-path disk))
          (cons "model" (device-model disk))
          (cons "serial" (device-serial disk))
          (cons "size" (device-size disk))))
  (map disk->json disks))
