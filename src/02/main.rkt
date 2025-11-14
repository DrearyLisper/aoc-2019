#lang racket

(require "../utils.rkt")
(require "../cpu.rkt")

(require threading)

(define (part01 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (let* ((program (parse)))
    (vector-set! program 1 12)
    (vector-set! program 2 2)
    (vector-ref (state-struct-program (execute program)) 0)))

(define (part02 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (~> (for*/list ((i (in-range 0 100))
                  (j (in-range 0 100)))
        (let* ((program (parse)))
          (begin
            (vector-set! program 1 i)
            (vector-set! program 2 j)
            (list (vector-ref (state-struct-program (execute program)) 0) i j))))
      (filter (lambda (l) (= (first l) 19690720)) _)
      (first)
      (match _
        ((list _ a b) (+ (* 100 a) b)))))

(define (main)
  (let ((content (read-input "02")))
    (println (part01 content))
    (println (part02 content))))

(module* main #f
  (main))
