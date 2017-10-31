;; run a child process and return a pair of input and output ports.
;; Executes the program 'command' with optional arguments 'args'
;; (all strings) in a subprocess.
;; Two ports to the process (based on pipes) are created and
;; returned.
;; The procedure is a modified version of the popen open-pipe*
;; procedure. Its functionality is close to that of
;; open-input-output-pipe. Changes are made to make it return two
;; ports instead of one in order to have a possibility to close
;; each one separately. This allows closing of the input pipe by
;; using (close-port port) when needed and emit EOF to the running
;; child process.
(define (open-io-pipe command . args)
  (let* ((c2p (pipe))  ; child to parent
         (p2c (pipe))) ; parent to child

    (setvbuf (cdr c2p) _IONBF)
    (setvbuf (cdr p2c) _IONBF)
    (let ((pid (primitive-fork)))
      (if (= pid 0)
        (begin
         ;; child process
         (ensure-batch-mode!)

         ;; select the three file descriptors to be used as
         ;; standard descriptors 0, 1, 2 for the new
         ;; process. They are pipes to/from the parent or taken
         ;; from the current Scheme input/output/error ports if
         ;; possible.

         (let ((input-fdes (fileno (car p2c)))
           (output-fdes (fileno (cdr c2p)))
           (error-fdes
             (or (false-if-exception (fileno (current-error-port)))
                 (open-fdes *null-device* O_WRONLY))))

           ;; close all file descriptors in ports inherited from
           ;; the parent except for the three selected above.
           ;; this is to avoid causing problems for other pipes in
           ;; the parent.

           ;; use low-level system calls, not close-port or the
           ;; scsh routines, to avoid side-effects such as
           ;; flushing port buffers or evicting ports.

           (port-for-each (lambda (pt-entry)
                (false-if-exception
                 (let ((pt-fileno (fileno pt-entry)))
                   (if (not (or (= pt-fileno input-fdes)
                        (= pt-fileno output-fdes)
                        (= pt-fileno error-fdes)))
                       (close-fdes pt-fileno))))))

           ;; Copy the three selected descriptors to the standard
           ;; descriptors 0, 1, 2, if not already there

           (if (not (= input-fdes 0))
             (begin
               (if (= output-fdes 0) (set! output-fdes (dup->fdes 0)))
               (if (= error-fdes  0) (set! error-fdes  (dup->fdes 0)))
               (dup2 input-fdes 0)
               ;; it's possible input-fdes is error-fdes
               (if (not (= input-fdes error-fdes))
                 (close-fdes input-fdes))))

           (if (not (= output-fdes 1))
             (begin
               (if (= error-fdes 1) (set! error-fdes (dup->fdes 1)))
               (dup2 output-fdes 1)
               ;; it's possible output-fdes is error-fdes
               (if (not (= output-fdes error-fdes))
                 (close-fdes output-fdes))))

           (if (not (= error-fdes 2))
             (begin
               (dup2 error-fdes 2)
               (close-fdes error-fdes)))

           (apply execlp command command args)))
        (begin
          ;; parent process
          ;; the forked child process should use these ports so
          ;; the parent process doesn't need them any more
          (close-port (cdr c2p))
          (close-port (car p2c))
          ;; return input and output ports
          (cons (car c2p) (cdr p2c))
          )))))

(define (read-process-string prog . args)
  (let* ((ports (apply open-io-pipe (cons prog args)))
         (input-port (cdr ports))
         (output-port (car ports)))
    (close-port input-port)
    (let ((output-string (get-string-all output-port)))
      (close-port output-port)
      output-string)))
