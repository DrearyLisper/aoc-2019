#lang racket

(require "../utils.rkt")
(require "../cpu.rkt")

(require threading)

(define (part01 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (let* ((program (parse)))
    (first (state-struct-output (execute-with-input program (list 1))))))

(define (part02 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (let* ((program (parse)))
    (first (state-struct-output (execute-with-input program (list 5))))))

(define (main)
  (let ((content (read-input "05")))
    (println (part01 content))
    (println (part02 content))))

(module* main #f
  (main))
