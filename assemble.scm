#lang racket


(define (assemble source-name)
  (let ((stdout #f)
        (rom-name (string-replace source-name ".asm" ".nes"))
        (ret-val #f))
    (set! stdout (with-output-to-string
                   (lambda ()
                     (set! ret-val (system* (find-executable-path "asm6")
                                            source-name rom-name "-l")))))
    (when (not ret-val)
          (printf "Error:\n~a" stdout))))


(let* ((fname (command-line #:args (input) input)))
  (assemble fname))
