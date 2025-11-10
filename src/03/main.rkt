#lang racket

(require "../utils.rkt")
(require threading)
(require racket/trace)
(require racket/hash)

(define (parse-line line)
  (~> (string-split line ",")
      (map (lambda (move) (list
                           (match (substring move 0 1) ("U" 'UP) ("R" 'RIGHT) ("L" 'LEFT) ("D" 'DOWN))
                           (string->number (substring move 1)))) _)))

(define (iterate s f n)
  (cond ((= n 0) '())
        (#t (cons (f s) (iterate (f s) f (- n 1))))))

(define (expand point move)
  (~> (match (first move)
        ('UP (lambda (p) (cons
                          (list (first (car p)) (- (second (car p)) 1))
                          (+ 1 (cdr p)))))
        ('DOWN (lambda (p) (cons
                            (list (first (car p)) (+ (second (car p)) 1))
                            (+ 1 (cdr p)))))
        ('LEFT (lambda (p) (cons
                            (list (- (first (car p)) 1) (second (car p)))
                            (+ 1 (cdr p)))))
        ('RIGHT (lambda (p) (cons
                             (list (+ (first (car p)) 1) (second (car p)))
                             (+ 1 (cdr p))))))
      (iterate point _ (second move))))

(define (foldl-step move s)
  (let* ((point (first s))
         (seen (second s))
         (points (expand point move))
         (new-point (last points))
         (new-seen (hash-union seen (make-immutable-hash points) #:combine (lambda (a b) a))))
    (list new-point new-seen)))


(define (part01 content)
  (let* ((lines (string-split content "\n"))
         (first-line (parse-line (first lines)))
         (second-line (parse-line (second lines)))
         (first-hash (second (foldl foldl-step (list (cons (list 0 0) 0) (make-immutable-hash)) first-line)))
         (second-hash (second (foldl foldl-step (list (cons (list 0 0) 0) (make-immutable-hash)) second-line))))
    (~> (hash-intersect first-hash second-hash #:combine (lambda (a b) (+ a b)))
        (hash-keys)
        (map (lambda (x) (+ (abs (first x)) (abs (second x)))) _)
        (sort _ <)
        (first))))


(define (part02 content)
  (let* ((lines (string-split content "\n"))
         (first-line (parse-line (first lines)))
         (second-line (parse-line (second lines)))
         (first-hash (second (foldl foldl-step (list (cons (list 0 0) 0) (make-immutable-hash)) first-line)))
         (second-hash (second (foldl foldl-step (list (cons (list 0 0) 0) (make-immutable-hash)) second-line))))
    (~> (hash-intersect first-hash second-hash #:combine (lambda (a b) (+ a b)))
        (hash-values)
        (sort _ <)
        (first))))

(define (main)
  (let ((content (read-input "03")))
    (println (part01 content))
    (println (part02 content))))

(module* main #f
  (main))
