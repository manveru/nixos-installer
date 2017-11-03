(use-modules (srfi srfi-64) ;; test framework
             (ice-9 textual-ports))

(load "lib/disks.scm")
(load "lib/tz.scm")

(let ((testname "tz-test"))
  (test-begin testname)
  (test-equal
    '(("country" "AD")
      ("coords" "+4230+00131")
      ("name" "Europe/Andorra"))
    (car (timezones)))
  (test-end testname))

(let ((testname "disk-detection-test"))
  (test-begin testname)
  (let* ((sample-output (get-string-all
                         (open-input-file
                          "tests/lsblk-sample-output.json")))
         (detected-disks (detect-disks-from-json
                          sample-output)))
    (begin
      (test-equal 1 (length detected-disks))
      (test-end testname))))

(let ((runner (test-runner-get)))
  (exit (if (< 0 (test-runner-fail-count runner))
            1
            0)))
