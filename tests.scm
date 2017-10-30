(use-modules (srfi srfi-64) ;; test framework
             (ice-9 textual-ports))

(load "lib/jq.scm")
(load "lib/disks.scm")

(let ((testname "jq-test"))
  (test-begin testname)
  (test-equal "0" (jq "[]" "length"))
  (test-end testname))

;; (let ((testname "disk-detection-test"))
;;   (test-begin testname)
;;   (let* ((sample-output (get-string-all
;;                          (open-input-file
;;                           "tests/lsblk-sample-output.json")))
;;          (detected-disks (detect-disks-from-json
;;                           sample-output)))
;;     (begin
;;       (test-assert (= 1 (length detected-disks)))
;;       (test-end testname))))

(let ((runner (test-runner-get)))
  (exit (if (< 0 (test-runner-fail-count runner))
            1
            0))
  )
