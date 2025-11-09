#lang racket

(provide execute)

(define (sum program ip)
  (let* ((a (vector-ref program (+ ip 1)))
         (b (vector-ref program (+ ip 2)))
         (o (vector-ref program (+ ip 3))))
    (vector-set! program o (+ (vector-ref program a) (vector-ref program b)))
    program))

(define (prod program ip)
  (let* ((a (vector-ref program (+ ip 1)))
         (b (vector-ref program (+ ip 2)))
         (o (vector-ref program (+ ip 3))))
    (vector-set! program o (* (vector-ref program a) (vector-ref program b)))
    program))


(define (execute program ip)
  (match (vector-ref program ip)
    (1 (execute (sum program ip) (+ ip 4)))
    (2 (execute (prod program ip) (+ ip 4)))
    (99 program)))
