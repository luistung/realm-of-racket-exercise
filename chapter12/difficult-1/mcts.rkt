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

    (define (visited? node)
      (not (zero? (mcts-node-n node))))

    (define (calc-t-v t-player t+1-player t-reward t+1-v)
      (define same-player? (equal? t-player t+1-player))
      (+ (* gamma t+1-v (if same-player? 1 -1)) t-reward))

    (define (greedy-choose node)
      (define children-list (hash->list (mcts-node-children node)))
      (argmax (lambda (a)
                (define child-node (cdr a))
                (define-values (w n) (values (mcts-node-w child-node) (mcts-node-n child-node)))
                (define pr (mcts-edge-reward (car a)))
                (calc-t-v (player (mcts-node-state node))
                          (player (mcts-node-state child-node))
                          pr
                          (/ w n 1.0)))
              children-list))

    (define (ucb-choose node)
      (define (ucb edge-node)
        (define N (mcts-node-n node))
        (define child-node (cdr edge-node))
        (define pr (mcts-edge-reward (car edge-node)))

        (cond
          [(not (visited? child-node)) +inf.0] ;;nerver come here
          [else
           (define-values (w n) (values (mcts-node-w child-node) (mcts-node-n child-node)))
           (define q
             (calc-t-v (player (mcts-node-state node))
                       (player (mcts-node-state child-node))
                       pr
                       (/ w n 1.0)))
           (+ q (* ucb-c (sqrt (/ (log N) n))))]))

      (define children-list (hash->list (mcts-node-children node)))
      (define not-visited-childern (filter (lambda (a) (not (visited? (cdr a)))) children-list))
      (cond
        [(pair? not-visited-childern) (first not-visited-childern)]
        [else (argmax (lambda (a) (ucb a)) children-list)]))

    (define (find-node-and-simulate node)
      (define children (mcts-node-children node))
      (define N (mcts-node-n node))
      (define (make-new-children node)
        (define state (mcts-node-state node))
        (define actions (possible-action state))
        (define rewards (map (curry process-reward state) actions))
        (define edges (map mcts-edge actions rewards))
        (define nodes (map (lambda (a) (mcts-node (take-action state a) 0 0 #f)) actions))
        (make-hash (map cons edges nodes)))

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

      (define (simulates state)
        (mean (for/list ([i per-simulate-num])
                (simulate state))))

      (define (simulate state)
        (cond
          [(terminal? state) 0]
          [else
           (define actions (possible-action state))
           (define normalize-scores (normalize (map (curry policy-score state) actions)))
           (define action (weighted-sample actions normalize-scores))
           (define new-state (take-action state action))
           (define same-player? (equal? (player state) (player new-state)))
           (calc-t-v (player state)
                     (player new-state)
                     (process-reward state action)
                     (simulate new-state))]))

      (define reward
        (cond
          [(or (not (visited? node)) (terminal? (mcts-node-state node)))
           (simulates (mcts-node-state node))]
          [else
           (unless children
             (set! children (make-new-children node))
             (set-mcts-node-children! node children))
           (define child-edge-node (ucb-choose node))
           (define child-node (cdr child-edge-node))
           (define same-player?
             (equal? (player (mcts-node-state node)) (player (mcts-node-state child-node))))
           (calc-t-v (player (mcts-node-state node))
                     (player (mcts-node-state child-node))
                     (mcts-edge-reward (car child-edge-node))
                     (find-node-and-simulate child-node))]))
      (set-mcts-node-n! node (add1 (mcts-node-n node)))
      (set-mcts-node-w! node (+ reward (mcts-node-w node)))
      reward)

    (define root (mcts-node node 0 0 #f))
    (for ([i simulate-num])
      (find-node-and-simulate root))

    (define edge-node (greedy-choose root))
    (mcts-edge-action (car edge-node))))
