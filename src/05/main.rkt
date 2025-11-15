#lang racket

(require "../utils.rkt")
(require "../cpu.rkt")

(require threading)

(define (part01 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (let* ((program (parse))
         (state (new-state-with-input program (list 1))))
    (first (state-struct-output (execute state)))))

(define (part02 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (let* ((program (parse))
         (state (new-state-with-input program (list 5))))
    (first (state-struct-output (execute state)))))

(define (main)
  (let ((content (read-input "05")))
    (println (part01 content))
    (println (part02 content))))

(module* main #f
  (main))
