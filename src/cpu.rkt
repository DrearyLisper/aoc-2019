#lang racket

(provide execute
         execute-until-output
         new-state
         new-state-with-input
         state-struct-program
         state-struct-halted
         set-state-struct-input!
         state-struct-output
         state-struct
         copy-state
         get-output)

(require threading)
(require racket/trace)

(struct opcode-struct (f args len) #:transparent)
(struct state-struct (program ip halted input output base memory) #:mutable)

(define (sum state opcode)
  (match-let* ((program (state-struct-program state))
               (ip (state-struct-ip state))
               ((list arg-a arg-b arg-c) (opcode-struct-args opcode))
               (a (vector-ref program (+ ip 1)))
               (b (vector-ref program (+ ip 2)))
               (o (vector-ref program (+ ip 3))))
    (write state (arg-c state o)
                 (+ (arg-a state a)
                    (arg-b state b)))
    (set-state-struct-ip! state (+ ip (opcode-struct-len opcode)))
    state))

(define (prod state opcode)
  (match-let* ((program (state-struct-program state))
               (ip (state-struct-ip state))
               ((list arg-a arg-b arg-c) (opcode-struct-args opcode))
               (a (vector-ref program (+ ip 1)))
               (b (vector-ref program (+ ip 2)))
               (o (vector-ref program (+ ip 3))))
    (write state (arg-c state o)
                 (* (arg-a state a)
                    (arg-b state b)))
    (set-state-struct-ip! state (+ ip (opcode-struct-len opcode)))
    state))


(define (halt state opcode)
  (set-state-struct-halted! state #t)
  state)

(define (input state opcode)
  (match-let* ((program (state-struct-program state))
               (input (state-struct-input state))
               (ip (state-struct-ip state))
               ((list arg-a) (opcode-struct-args opcode))
               (a (vector-ref program (+ ip 1))))
    (write state (arg-a state a) (first input))
    (set-state-struct-input! state (rest input))
    (set-state-struct-ip! state (+ ip (opcode-struct-len opcode)))
    state))

(define (output state opcode)
  (match-let* ((program (state-struct-program state))
               (output (state-struct-output state))
               (ip (state-struct-ip state))
               ((list arg-a) (opcode-struct-args opcode))
               (a (vector-ref program (+ ip 1))))
    (set-state-struct-output! state (cons (arg-a state a) output))
    (set-state-struct-ip! state (+ ip (opcode-struct-len opcode)))
    state))

(define (jump-if-true state opcode)
  (match-let* ((program (state-struct-program state))
               (output (state-struct-output state))
               (ip (state-struct-ip state))
               ((list arg-a arg-b) (opcode-struct-args opcode))
               (a (vector-ref program (+ ip 1)))
               (b (vector-ref program (+ ip 2))))
    (if (not (equal? (arg-a state a) 0))
        (set-state-struct-ip! state (arg-b state b))
        (set-state-struct-ip! state (+ ip (opcode-struct-len opcode))))
    state))

(define (jump-if-false state opcode)
  (match-let* ((program (state-struct-program state))
               (output (state-struct-output state))
               (ip (state-struct-ip state))
               ((list arg-a arg-b) (opcode-struct-args opcode))
               (a (vector-ref program (+ ip 1)))
               (b (vector-ref program (+ ip 2))))
    (if (equal? (arg-a state a) 0)
        (set-state-struct-ip! state (arg-b state b))
        (set-state-struct-ip! state (+ ip (opcode-struct-len opcode))))
    state))

(define (less-than state opcode)
  (match-let* ((program (state-struct-program state))
               (output (state-struct-output state))
               (ip (state-struct-ip state))
               ((list arg-a arg-b arg-c) (opcode-struct-args opcode))
               (a (vector-ref program (+ ip 1)))
               (b (vector-ref program (+ ip 2)))
               (c (vector-ref program (+ ip 3))))

    (if (< (arg-a state a) (arg-b state b))
        (write state (arg-c state c) 1)
        (write state (arg-c state c) 0))
    (set-state-struct-ip! state (+ ip (opcode-struct-len opcode)))
    state))

(define (equals state opcode)
  (match-let* ((program (state-struct-program state))
               (output (state-struct-output state))
               (ip (state-struct-ip state))
               ((list arg-a arg-b arg-c) (opcode-struct-args opcode))
               (a (vector-ref program (+ ip 1)))
               (b (vector-ref program (+ ip 2)))
               (c (vector-ref program (+ ip 3))))

    (if (equal? (arg-a state a) (arg-b state b))
        (write state (arg-c state c) 1)
        (write state (arg-c state c) 0))
    (set-state-struct-ip! state (+ ip (opcode-struct-len opcode)))
    state))

(define (base state opcode)
  (match-let* ((program (state-struct-program state))
               (ip (state-struct-ip state))
               ((list arg-a) (opcode-struct-args opcode))
               (a (vector-ref program (+ ip 1))))
    (set-state-struct-base! state (+ (state-struct-base state) (arg-a state a)))
    (set-state-struct-ip! state (+ ip (opcode-struct-len opcode)))
    state))

(define (read state addr)
  (let* ((program (state-struct-program state))
         (memory (state-struct-memory state)))
    (if (< addr (vector-length program))
        (vector-ref program addr)
        (hash-ref! memory addr 0))))

(define (write state addr v)
  (let* ((program (state-struct-program state))
         (memory (state-struct-memory state)))
    (if (< addr (vector-length program))
        (vector-set! program addr v)
        (hash-set! memory addr v))))

(define (parse-args opcode type)

  (define (select-mode mode)
    (match mode
      (0 (lambda (state addr) (read state addr)))
      (1 (lambda (state addr) addr))
      (2 (lambda (state addr) (read state
                                    (+ addr (state-struct-base state)))))
      (3 (lambda (state addr) (+ addr (state-struct-base state))))))


  (define (mode-for-position position)
    (match position
      (0 (remainder (quotient opcode 100) 10))
      (1 (remainder (quotient opcode 1000) 10))
      (2 (remainder (quotient opcode 10000) 10))))

  (match type
    ('sum (map select-mode (list
                            (mode-for-position 0)
                            (mode-for-position 1)
                            (+ 1 (mode-for-position 2)))))
    ('prod (map select-mode (list (mode-for-position 0)
                                  (mode-for-position 1)
                                  (+ 1 (mode-for-position 2)))))
    ('halt (list))
    ('input (map select-mode (list (+ 1 (mode-for-position 0)))))
    ('output (map select-mode (list (mode-for-position 0))))
    ('jump-if-true (map select-mode (list (mode-for-position 0) (mode-for-position 1))))
    ('jump-if-false (map select-mode (list (mode-for-position 0) (mode-for-position 1))))
    ('less-than (map select-mode (list
                                  (mode-for-position 0)
                                  (mode-for-position 1)
                                  (+ 1 (mode-for-position 2)))))
    ('equals (map select-mode (list
                               (mode-for-position 0)
                               (mode-for-position 1)
                               (+ 1 (mode-for-position 2)))))
    ('base (map select-mode (list (mode-for-position 0))))))

(define (parse-opcode opcode)
  (match (remainder opcode 100)
    (1 (opcode-struct sum (parse-args opcode 'sum) 4))
    (2 (opcode-struct prod (parse-args opcode 'prod) 4))
    (3 (opcode-struct input (parse-args opcode 'input) 2))
    (4 (opcode-struct output (parse-args opcode 'output) 2))
    (5 (opcode-struct jump-if-true (parse-args opcode 'jump-if-true) 3))
    (6 (opcode-struct jump-if-false (parse-args opcode 'jump-if-false) 3))
    (7 (opcode-struct less-than (parse-args opcode 'less-than) 4))
    (8 (opcode-struct equals (parse-args opcode 'equals) 4))
    (9 (opcode-struct base (parse-args opcode 'base) 2))
    (99 (opcode-struct halt (parse-args opcode 'halt) 1))))

(define (execute-opcode state)
  (let* ((opcode-raw (vector-ref (state-struct-program state) (state-struct-ip state)))
         (opcode (parse-opcode opcode-raw)))
    ((opcode-struct-f opcode) state opcode)))

(define (new-state program)
  (state-struct program 0 #f (list) (list) 0 (make-hash)))

(define (new-state-with-input program input)
  (state-struct program 0 #f input (list) 0 (make-hash)))

(define (copy-list l)
  (cond ((empty? l) '())
        (#t (cons (first l) (copy-list (rest l))))))

(define (copy-state state)
  (state-struct
   (vector-copy (state-struct-program state))
   (state-struct-ip state)
   (state-struct-halted state)
   (copy-list (state-struct-input state))
   (copy-list (state-struct-output state))
   (state-struct-base state)
   (hash-copy (state-struct-memory state))))

(define (get-output state)
  (reverse (state-struct-output state)))

(define (execute state)
  (define (execute-impl state)
    (cond ((state-struct-halted state) state)
          (#t (execute-impl (execute-opcode state)))))

  (execute-impl state))

(define (execute-until-output state)
  (define (execute-impl state)
    (cond ((or (state-struct-halted state)
               (not (empty? (state-struct-output state)))) state)
          (#t (execute-impl (execute-opcode state)))))

  (set-state-struct-output! state (list))
  (execute-impl state))
