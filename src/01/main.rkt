#lang racket

(require "../utils.rkt")

(define (part01 content)
  (define (module->fuel module)
    (- (floor (/ module 3)) 2))
  (let* ((modules (string-split content "\n"))
         (modules (map string->number modules))
         (fuel (map module->fuel modules)))
    (foldl + 0 fuel)))

(define (part02 content)
  (define (module->fuel module)
    (let ((fuel (- (floor (/ module 3)) 2)))
      (cond ((<= fuel 0) 0)
            (#t (+ fuel (module->fuel fuel))))))

  (let* ((modules (string-split content "\n"))
         (modules (map string->number modules))
         (fuel (map module->fuel modules)))

    (foldl + 0 fuel)))


(define (main)
  (let ((content (read-input "01")))
    (println (part01 content))
    (println (part02 content))))

(module* main #f
  (main))
