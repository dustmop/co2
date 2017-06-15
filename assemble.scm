#lang racket

(define (assemble source-name [rom-name #f])
  (let ((stdout #f)
        (ret-val #f))
    (when (not rom-name)
          (set! rom-name (string-replace source-name ".asm" ".nes")))
    (set! stdout (with-output-to-string
                   (lambda ()
                     (set! ret-val (system* (find-executable-path "asm6")
                                            source-name rom-name "-l")))))
    (when (not ret-val)
          (error "Error:\n~a" stdout))))

(provide assemble)
