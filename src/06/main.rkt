#lang racket

(require "../utils.rkt")
(require threading)
(require racket/hash)

(define (part01 content)
  (define (orbits hash planet)
    (cond ((equal? planet "COM") 0)
          (#t (+ 1 (orbits hash (first (hash-ref hash planet)))))))

  (let* ((lines (string-split content "\n"))
         (pairs (map (lambda (x) (reverse (string-split x ")"))) lines))
         (hash (make-immutable-hash pairs)))
    (~> (hash-keys hash)
        (map (lambda (x) (orbits hash x)) _)
        (apply + _))))

(define (part02 content)
  (define (first-or-default l d)
    (if (empty? l) d (first l)))

  (define (dfs hash planet seen)
    (cond
     ((equal? planet "SAN") seen)
     (#t (let* ((planets (hash-ref hash planet))
                (unseen-planets (set->list (set-subtract (apply set planets) seen))))
           (if (empty? unseen-planets)
               '()
               (first-or-default (filter
                                  (lambda (x) (not (set-empty? x)))
                                  (for/list ((next-planet unseen-planets))
                                    (dfs hash next-planet (set-add seen next-planet)))) '()))))))

  (let* ((lines (string-split content "\n"))
         (pairs (map (lambda (x) (make-immutable-hash (list (reverse (string-split x ")")) (string-split x ")")))) lines))
         (hash (apply hash-union pairs #:combine/key (lambda (k v1 v2) (append v1 v2)))))
    (- (set-count (dfs hash "YOU" (set))) 2)))

(define (main)
  (let ((content (read-input "06")))
    (println (part01 content))
    (println (part02 content))))

(module* main #f
  (main))
