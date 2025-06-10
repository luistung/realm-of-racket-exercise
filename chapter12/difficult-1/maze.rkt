#lang racket

(require (submod "mcts.rkt" mcts-generics))
(require (submod "mcts.rkt" mcts))

(define MAZE-STATE-WIDTH (make-parameter #f))
(define MAZE-STATE-HEIGHT (make-parameter #f))
(define policy-prob (make-parameter #f))
(define local-reward (make-parameter #f))

(struct maze-state (x y player)
  #:transparent
  #:methods gen:mcts-state
  [(define (player mcts-state)
     (maze-state-player mcts-state))
   (define (possible-action state)
     (define-values (x y) (values (maze-state-x state) (maze-state-y state)))
     (for/list ([action '(left right up down)]
                [valid? (list (> x 0)
                              (< x (sub1 (MAZE-STATE-WIDTH)))
                              (> y 0)
                              (< y (sub1 (MAZE-STATE-HEIGHT))))]
                #:when valid?)
       action))
   (define (terminal? state)
     (and (= (maze-state-x state) (sub1 (MAZE-STATE-WIDTH)))
          (= (maze-state-y state) (sub1 (MAZE-STATE-HEIGHT)))))
   ;;imperfect policy
   (define (policy-score state action)
     (case action
       [(right down) (policy-prob)]
       [else (- 1 (policy-prob))]))
   (define (process-reward state action)
     (if (terminal? (take-action state action)) 1.0 (local-reward)))
   (define (take-action state action)
     (define x (maze-state-x state))
     (define y (maze-state-y state))
     (define player (maze-state-player state))
     (case action
       ['left (maze-state (sub1 x) y player)]
       ['right (maze-state (add1 x) y player)]
       ['up (maze-state x (sub1 y) player)]
       ['down (maze-state x (add1 y) player)]
       [else (error action "error")]))])

(define (init-state width height #:player [player 0])
  (MAZE-STATE-WIDTH width)
  (MAZE-STATE-HEIGHT height)
  (maze-state 0 0 player))

(module+ test
  (require rackunit)
  (define average-step-num
    (thunk (define episode-num 100)
           (policy-prob 0.5)
           (local-reward -0.5)
           (/ (for/sum ([i episode-num])
                       (random-seed i)
                       (define state (init-state 3 3))
                       (let loop ([state state]
                                  [step 0])
                         (cond
                           [(or (terminal? state) (>= step 100)) step]
                           [else
                            (define action
                              (mcts-search state #:simulate-num 10 #:per-simulate-num 1 #:gamma 0.9))
                            #;(displayln state)
                            #;(displayln action)
                            (loop (take-action state action) (add1 step))])))
              episode-num)))
  (check-equal? (average-step-num) (/ 472 100))
  "maze tested")