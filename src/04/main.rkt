#lang racket

(require "../utils.rkt")
(require threading)

(define (part01 content)

  (define (is-password? n)
    (let* ((n-str (number->string n))
           (digits (map (lambda (x) (- (char->integer x) (char->integer #\0))) (string->list n-str)))
           (pairs (map list (drop-right digits 1) (rest digits)))
           (a (andmap (lambda (x) (<= (first x) (second x))) pairs))
           (b (ormap (lambda (x) (= (first x) (second x))) pairs)))
      (and a b)))

  (let* ((parts (map string->number (string-split content "-")))
         (l (first parts))
         (r (second parts)))
    (stream-length (stream-filter identity (stream-map is-password? (in-range l r))))))

(define (part02 content)

  (define (is-password? n)
    (let* ((n-str (number->string n))
           (digits (map (lambda (x) (- (char->integer x) (char->integer #\0))) (string->list n-str)))
           (pairs (map list (drop-right digits 1) (rest digits)))
           (a (andmap (lambda (x) (<= (first x) (second x))) pairs))
           (b (append (cons #f (map (lambda (x) (= (first x) (second x))) pairs)) '(#f)))
           (b-pairs (map list (drop-right b 2) (drop-right (rest b) 1) (rest (rest b))))
           (b-goods (length (filter (lambda (x) (equal? x '(#f #t #f))) b-pairs))))
      (and a (> b-goods 0))))

  (let* ((parts (map string->number (string-split content "-")))
         (l (first parts))
         (r (second parts)))
    (stream-length (stream-filter identity (stream-map is-password? (in-range l r))))))

(define (main)
  (let ((content "136818-685979"))
    (println (part01 content))
    (println (part02 content))))

(module* main #f
  (main))
