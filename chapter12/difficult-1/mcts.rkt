#lang racket

(module mcts-generics racket
  (require racket/generic)
  (provide gen:mcts-state
           player
           possible-action
           take-action
           terminal?
           process-reward
           policy-score)
  (define-generics mcts-state
                   (player mcts-state)
                   (possible-action mcts-state)
                   (take-action mcts-state action)
                   (terminal? mcts-state)
                   (process-reward mcts-state action)
                   (policy-score mcts-state action)))

(module maze racket
  (provide init-state
           policy-prob
           local-reward)

  (require (submod ".." mcts-generics))

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
       (if (terminal? (take-action state action))
           1.0
           (local-reward)))
     
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
    (maze-state 0 0 player)))

(module mcts racket
  (provide mcts-search)
  (require (submod ".." mcts-generics))
  (require math/statistics)

  (struct mcts-node (state [w #:mutable] [n #:mutable] [children #:mutable]) #:transparent)
  (struct mcts-edge (action reward) #:transparent)

  (define (mcts-print root)
    (display "(")
    (printf "~a ~a " (mcts-node-w root) (mcts-node-n root))
    (map (lambda (a)
           (define edge (car a))
           (define node (cdr a))
           (display "(")
           (printf "~a ~a " (mcts-edge-action edge) (mcts-edge-reward edge))
           (mcts-print node)
           (display ")"))
         (hash->list (or (mcts-node-children root) (make-hash))))
    (display ")"))

  (define (mcts-search node
                       #:simulate-num [simulate-num 10]
                       #:per-simulate-num [per-simulate-num 1]
                       #:gamma [gamma 0.8]
                       #:ucb-c [ucb-c (sqrt 2)])
    (define root (mcts-node node 0 0 #f))

    (define (visited? node)
      (not (zero? (mcts-node-n node))))

    (define (greedy-choose node)
      (define children-list (hash->list (mcts-node-children node)))
      (argmax (lambda (a)
                (define node (cdr a))
                (define-values (w n) (values (mcts-node-w node) (mcts-node-n node)))
                (define pr (mcts-edge-reward (car a)))
                (+ pr (/ w n 1.0)))
              children-list))

    (define (ucb-choose node)
      (define (ucb edge-node N)
        (define node (cdr edge-node))
        (define pr (mcts-edge-reward (car edge-node)))

        (cond
          [(not (visited? node)) +inf.0] ;;nerver come here
          [else
           (define-values (w n) (values (mcts-node-w node) (mcts-node-n node)))
           (+ pr (/ w n 1.0) (* ucb-c (sqrt (/ (log N) n))))]))

      (define children-list (hash->list (mcts-node-children node)))
      (define not-visited-childern (filter (lambda (a) (not (visited? (cdr a)))) children-list))
      (cond
        [(pair? not-visited-childern) (first not-visited-childern)]
        [else
         (define N (mcts-node-n node))
         (argmax (lambda (a) (ucb a N)) children-list)]))

    (define (find-path-to-simulate edge node)
      (define children (mcts-node-children node))
      (define N (mcts-node-n node))
      (define (make-new-children node)
        (define state (mcts-node-state node))
        (define actions (possible-action state))
        (define rewards (map (curry process-reward state) actions))
        (define edges (map mcts-edge actions rewards))
        (define nodes (map (lambda (a) (mcts-node (take-action state a) 0 0 #f)) actions))
        (make-hash (map cons edges nodes)))

      (cond
        [(or (not (visited? node)) (terminal? (mcts-node-state node))) (list (cons edge node))]
        [else
         (unless children
           (set! children (make-new-children node))
           (set-mcts-node-children! node children))
         (define child-edge-node (ucb-choose node))
         (cons (cons edge node)
               (find-path-to-simulate (car child-edge-node) (cdr child-edge-node)))]))

    (define (update-path path reward)
      (let loop ([reverse-path (reverse path)]
                 [reward reward])
        (unless (empty? reverse-path)
          (define edge-node (car reverse-path))
          (define node (cdr edge-node))
          (define edge (car edge-node))
          (set-mcts-node-n! node (add1 (mcts-node-n node)))
          (set-mcts-node-w! node (+ reward (mcts-node-w node)))
          (loop (cdr reverse-path)
                (+ (if edge
                       (mcts-edge-reward edge)
                       0)
                   (* gamma reward))))))

    (for ([i simulate-num])
      (define path-to-simulate (find-path-to-simulate #f root))

      (define node-to-simulate
        (cdr (for/last ([i path-to-simulate])
               i)))
      (define (normalize scores)
        (define s (apply + scores))
        (map (lambda (a) (/ a s 1.0)) scores))

      (define (weighted-sample l w)
        (define r (random))
        (for/fold ([acc 0.0]
                   [ret #f]
                   #:result ret)
                  ([i l]
                   [j w])
          #:break (> acc r)
          (values (+ acc j) i)))

      (define (simulate state)
        (cond
          [(terminal? state) 0]
          [else
           (define actions (possible-action state))
           (define normalize-scores (normalize (map (curry policy-score state) actions)))
           (define action (weighted-sample actions normalize-scores))
           (define new-state (take-action state action))
           (define same-player? (equal? (player state) (player new-state)))
           (+ (* gamma (simulate new-state) (if same-player? 1 -1)) (process-reward state action))]))

      (define reward
        (mean (for/list ([i per-simulate-num])
                (simulate (mcts-node-state node-to-simulate)))))

      (update-path path-to-simulate reward))

    (define edge-node (greedy-choose root))
    (mcts-edge-action (car edge-node))))

(require (submod "." mcts))
(require (submod "." maze))
(require (submod "." mcts-generics))

(define episode-num 100)
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
   episode-num
   1.0)