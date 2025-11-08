#lang racket

(require racket/port)

(provide read-test read-input)

(define (read-test day)
  (let* ((path (format "src/~a/test.txt" day)))
    (port->string (open-input-file path))))

(define (read-input day)
  (let* ((path (format "src/~a/input.txt" day)))
    (port->string (open-input-file path))))
