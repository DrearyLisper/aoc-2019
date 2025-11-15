#lang racket

(require "../utils.rkt")
(require "../cpu.rkt")

(require threading)
(require racket/trace)

(define (part01 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (define (execute-amp a b)
    (let* ((program (parse))
          (state (new-state-with-input program (list a b))))
      (first (state-struct-output (execute-until-output state)))))

  (define (bruteforce n b)
    (if (empty? n) (list b)
        (foldl append '() (map
                           (lambda (a-and-next-b) (bruteforce
                                                   (remove (first a-and-next-b) n)
                                                   (second a-and-next-b)))
                           (for/list ((a n))
                             (list a (execute-amp a b)))))))

  (let* ((program (parse)))

    (apply max (bruteforce '(0 1 2 3 4) 0))))

(define (part02 content)
  (define (parse)
    (list->vector (map string->number (string-split (string-trim content) ","))))

  (define (execute-amp a b-and-states)
    (let* ((b (first b-and-states))
           (states (second b-and-states))
           (program (parse))
           (state (new-state-with-input program (list a b))))
      (list (first (state-struct-output (execute-until-output state)))
            (cons state (map copy-state states)))))

  (define (bruteforce n b)
    (if (empty? n) (list b)
        (foldl append '() (map
                           (lambda (a-and-next-b) (bruteforce
                                                   (remove (first a-and-next-b) n)
                                                   (second a-and-next-b)))
                           (for/list ((a n))
                             (list a (execute-amp a b)))))))

  (define (continue output-and-states-and-as)
    (let* ((output (first output-and-states-and-as))
           (states (reverse (second output-and-states-and-as)))
           (outputs (list output)))

      (define (first-or l d)
        (if (empty? l) d (first l)))

      (define (continue-iter)
        (for ((state states))
          #:break (equal? output #f)

          (set-state-struct-input! state (list output))
          (set! output (first-or
                        (state-struct-output (execute-until-output state))
                        #f)))

        (when output
          (set! outputs (cons output outputs)))

        (if output
            (continue-iter)
            outputs))

      (first (continue-iter))))

  (let* ((program (parse))
         (bruteforces (bruteforce '(9 8 7 6 5) (list 0 '()))))
    (apply max (map continue bruteforces))))

(define (main)
  (let ((content (read-input "07")))
    (println (part01 content))
    (println (part02 content))))

(module* main #f
  (main))
