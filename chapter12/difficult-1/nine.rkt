#lang racket

(require (submod "mcts.rkt" mcts-generics))
(require (submod "mcts.rkt" mcts))

(define-values (nine-board
                nine-board?
                nine-board-init
                nine-board-copy
                nine-board-set!
                nine-board-set-xy!
                nine-board-get
                nine-board-get-xy
                nine-board-possible
                nine-board-terminal?
                nine-board-print)
  (let ()
    (struct nine-board (v) #:transparent)
    (define (have-equal? l)
      (if (and (not (= (car l) -1)) (apply = l)) (car l) #f))
    (values nine-board
            nine-board?
            (lambda () (nine-board (make-vector 9 -1)))
            (lambda (s) (nine-board (vector-copy (nine-board-v s))))
            (lambda (s pos player) (vector-set! (nine-board-v s) pos player))
            (lambda (s x y player) (nine-board-set! s (+ (* x 3) y) player))
            (lambda (s pos) (vector-ref (nine-board-v s) pos))
            (lambda (s x y) (vector-ref (nine-board-v s) (+ (* x 3) y)))
            (lambda (s)
              (define v (nine-board-v s))
              (for/list ([i (in-naturals)]
                         [j (in-vector v)]
                         #:when (= j -1))
                i))
            (lambda (s)
              (define v (nine-board-v s))
              (or (for/or ([x 3])
                    (have-equal? (for/list ([y 3])
                                   (vector-ref v (+ (* x 3) y)))))
                  (for/or ([y 3])
                    (have-equal? (for/list ([x 3])
                                   (vector-ref v (+ (* x 3) y)))))
                  (have-equal? (for/list ([x 3])
                                 (vector-ref v (+ (* x 3) x))))
                  (have-equal? (for/list ([x 3])
                                 (vector-ref v (+ (* x 3) (- 2 x)))))
                  (if (for/or ([i v])
                        (= i -1))
                      #f
                      -1)))
            (lambda (s)
              (define v (nine-board-v s))
              (define (convert-char c)
                (case c
                  [(0) "O"]
                  [(1) "X"]
                  [else "-"]))
              (for ([i (in-naturals)]
                    [j v])
                (display (convert-char j))
                (when (= (modulo i 3) 2)
                  (displayln "")))))))

(struct nine-state (board player)
  #:transparent
  #:methods gen:mcts-state
  [(define (player mcts-state)
     (nine-state-player mcts-state))
   (define (possible-action mcts-state)
     (nine-board-possible (nine-state-board mcts-state)))
   (define (take-action mcts-state action)
     (define player (nine-state-player mcts-state))
     (define new-board (nine-board-copy (nine-state-board mcts-state)))
     (nine-board-set! new-board action player)
     (nine-state new-board (- 1 player)))
   (define (terminal? mcts-state)
     (nine-board-terminal? (nine-state-board mcts-state)))
   (define (process-reward mcts-state action)
     (define new-state (take-action mcts-state action))
     (define terminal-player (terminal? new-state))
     (define player (nine-state-player mcts-state))
     #;(displayln new-state)
     #;(printf "~a ~a\n" player terminal-player)
     (cond
       [(not terminal-player) 0.0]
       [(= terminal-player -1) 0.0]
       [(= terminal-player player) 1.0]
       [else -1.0]))
   (define (policy-score mcts-state action)
     1.0)])

(define (init-state)
  (nine-state (nine-board-init) 0)
  #;(nine-state (nine-board (vector 0 -1 0 1 1 0 1 0 1)) 0))

(define (random-choose l)
  (define len (length l))
  (list-ref l (random len)))

(module+ test
  (require rackunit)
  (define win-count
    (count identity
           (for/list ([i 100])
             (random-seed i)
             (define state (init-state))
             (define first-player (random 2))

             (equal?
              first-player
              (let loop ([state state]
                         [step 0])
                (define terminal-player (terminal? state))
                (cond
                  [(or terminal-player) (if (= terminal-player -1) 'tie terminal-player)]
                  [else
                   (define action
                     (if (= (modulo step 2) first-player)
                         (mcts-search state #:simulate-num 10 #:per-simulate-num 100 #:gamma 0.9)
                         (random-choose (possible-action state))))
                   (loop (take-action state action) (add1 step))]))))))
  (check-equal? win-count 90))