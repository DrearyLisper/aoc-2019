#lang racket

(require "../utils.rkt")
(require "../cpu.rkt")

(require threading)
(require racket/trace)

(define (part01 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (first (get-output (execute (new-state-with-input (parse) (list 1))))))

(define (part02 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (first (get-output (execute (new-state-with-input (parse) (list 2))))))

(define (main)
  (let ((content (read-input "09")))
    (println (part01 content))
    (println (part02 content))))

(module* main #f
  (main))
