#lang racket

(require "../utils.rkt")
(require threading)
(require racket/hash)

(define (chunk l n)
  (cond ((empty? l) '())
        (#t (cons (take l n) (chunk (drop l n) n)))))

(define (part01 content)
  (define (find c chunk)
    (count (lambda (x) (equal? c x)) chunk))

  (let* ((chunks (chunk (string->list (string-trim content)) (* 25 6)))
         (stats (map (lambda (chunk) (list
                                      (find #\0 chunk)
                                      (find #\1 chunk)
                                      (find #\2 chunk))) chunks)))
    (match (argmin first stats)
      ((list a b c) (* b c)))))

(define (part02 content)
  (define (combine b a)
    (for/list ((i a) (j b))
      (match (list i j)
        ((list #\0 _) #\0)
        ((list #\1 _) #\1)
        ((list _ b) b))))

  (let* ((chunks (chunk (string->list (string-trim content)) (* 25 6)))
         (image (foldl combine (first chunks) (rest chunks))))
    (for ((row (chunk image 25)))
      (~> row
          (map (lambda (x) (match x (#\0 #\space) (#\1 #\#) (c c))) _)
          (list->string _)
          (displayln _)))))

(define (main)
  (let ((content (read-input "08")))
    (println (part01 content))
    (part02 content)))

(module* main #f
  (main))
