#lang racket

#|
   The Dice of Doom game, the eager version
   ----------------------------------------

   The Dice of Doom game is a turn-based game for two players sharing one keyboard.
   Since this implementation employs an eager strategy to build the complete game
   tree of all possible moves, it is only a step in the right direction.

   Each player owns hexagonal territories, which are arranged into a planar game
   board. A territory comes with a number of dice. When it is a player's turn,
   she marks one of her territories as a launching pad for an attack at a
   neigboring territory of the other player. Such an attack is enabled only if
   her chosen territory has more dice than the territory of the other player.
   The effect of the attack is that the territory changes ownership and that all
   but one of the attack dice are moved to the newly conquered territory. A
   player may continue her turn as long as she can launch attacks. Optionally,
   she may choose to pass after her first attack is executed, meaning she ends
   her turn. At the end of a turn, a number of dices are distributed across the
   players' territories. The game is over when a player whose turn it is cannot
   attack on her first move.

   A player can use the following five keys to play the game:
    -- with ← and → (arrow keys), the player changes the territory focus
    -- with enter, the player marks a territory the launching pad for an attack
    -- with the "d" key, the player unmarks a territory
    -- with the "p" key the player passes.
   Once a player passes, the game announces whose turn it is next.

   Play
   ----

   Run and evaluate
     (roll-the-dice)
   This will pop up a window that the game board, and instructions.
|#

(require 2htdp/image
         (except-in 2htdp/universe left right))
(require racket/random)

;
;
;
;
;   ;;;;       ;                            ;;; ;;;                   ;;        ;;
;    ;  ;                                    ;   ;                     ;         ;
;    ;   ;   ;;;     ;;; ;   ;;;;            ;   ;   ;;;;   ;; ;;;     ;     ;;; ;
;    ;   ;     ;    ;   ;;  ;    ;           ; ; ;  ;    ;   ;;        ;    ;   ;;
;    ;   ;     ;    ;       ;;;;;;           ; ; ;  ;    ;   ;         ;    ;    ;
;    ;   ;     ;    ;       ;                ; ; ;  ;    ;   ;         ;    ;    ;
;    ;  ;      ;    ;    ;  ;                ; ; ;  ;    ;   ;         ;    ;   ;;
;   ;;;;     ;;;;;   ;;;;    ;;;;;            ; ;    ;;;;   ;;;;;    ;;;;;   ;;; ;;
;
;
;
;

;; ---------------------------------------------------------------------------------------------------
;; Data

(struct dice-world (src gt) #:transparent)
(define (dice-world-board w) (game-board (dice-world-gt w)))
;; DiceWorld = (dice-world (U #false Natural) Board GameTree)
;; in (dice-world i b gt)
;; -- if i is a Natural, it is an index for the territory that the player has marked for an attack
;; -- if i is #f, no territory has been marked yet
;; b is the current board
;; gt is the game-tree for the given i and b

(struct game (board player moves) #:transparent)
;; GameTree = (game Board Player [Listof Move])
;; in (game-tree b p lm)
;; -- b is the current board
;; -- p is the current player
;; -- lm is the list of moves that that player may execute

;; Board = [List-of Territory]
;; the first field in the list is the currently marked  territory

;; Player ∈ [0, PLAYER#) | Natural

(struct move (action gt) #:transparent)
;; Move = (move Action GameTree)
;; in (move a gt)
;; -- a represents the actione to be takem
;; -- gt is the game-tree resulting from that action

;; Action is one of:
;; -- '()                      a passing move
;; -- (list Natural Natural)   the move where the first attacks the second

(struct territory (index player dice x y) #:transparent)
;; Territory = (territory Natural Player Dice Integer Integer)
;; in (territory i p d x y)
;; -- i is a unique identifier for the territory; it also determines its initial location
;; -- p is the player who owns this territory
;; -- d is the number of dice on this board
;; -- x is the x coordiate of this territory in pixels
;; -- y is the y coordiate of this territory in pixels

;; Territory Natural -> Territory
;; updates number of dice on territory
(define (territory-set-dice t d)
  (territory (territory-index t) (territory-player t) d (territory-x t) (territory-y t)))

;; Territory Player -> Territory
;; updates owner of territory
(define (territory-set-player t p)
  (territory (territory-index t) p (territory-dice t) (territory-x t) (territory-y t)))

;; ---------------------------------------------------------------------------------------------------
;; Constants

; initalization constants
(define PLAYER# 2)
(define DICE# 3)
(define BOARD 2)
(define GRID (* BOARD BOARD))
(define INIT-PLAYER 0)
(define INIT-SPARE-DICE 10)
; The depth at which to limit the gametree
(define AI-DEPTH 4)
(define AI 1)

; graphical constants: territories
(define DICE-OFFSET 6)
(define SIDE 75)
(define OFFSET0 (* 2 SIDE))
(define ROTATION 30)
(define HEX 6)
(define (hexagon color)
  (rotate ROTATION (regular-polygon SIDE HEX "solid" color)))
(define X-OFFSET (image-width (hexagon "black")))
(define Y-OFFSET (* (image-height (hexagon "black")) 3/4))

; graphical constants
(define COLORS (list (make-color 255 0 0 100) (make-color 0 255 0 100) (make-color 0 0 255 100)))
(define FOCUS (rotate ROTATION (regular-polygon SIDE 6 "outline" "black")))
(define D1 (bitmap "graphics/dice1.png"))
(define D2 (bitmap "graphics/dice2.png"))
(define D3 (bitmap "graphics/dice3.png"))
(define D4 (bitmap "graphics/dice4.png"))
(define IMG-LIST (list D1 D2 D3 D4))

(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define INSTRUCT "← and → to move among territories, <enter> to mark, <d> to unmark, and <p> to pass")
(define AI-TURN "It's the Mighty AI's turn")
(define YOUR-TURN "It's your turn")
(define INFO-X-OFFSET 100)
(define INFO-Y-OFFSET 50)

(define INSTRUCTIONS (text INSTRUCT TEXT-SIZE TEXT-COLOR))
(define WIDTH (+ (image-width INSTRUCTIONS) 50))
(define HEIGHT 600)
(define (PLAIN)
  (define iw (image-width INSTRUCTIONS))
  (define bw (* SIDE 2 BOARD))
  (set! WIDTH (+ (max iw bw) 50))
  (set! HEIGHT (+ (* SIDE 2 BOARD) 50))
  (empty-scene WIDTH HEIGHT))
(define (ISCENE)
  (define mt (PLAIN))
  (when (or (> (image-width mt) 1280) (> (image-height mt) 800))
    (error 'scene
           "it is impossible to draw a ~s x ~s game scene for a 1280 x 800 laptop screen"
           (image-width mt)
           (image-height mt)))
  (place-image INSTRUCTIONS (* .5 WIDTH) (* .9 HEIGHT) mt))

;
;
;
;
;   ;;; ;;;            ;
;    ;; ;;
;    ;; ;;   ;;;;    ;;;    ;; ;;
;    ; ; ;  ;    ;     ;     ;;  ;
;    ; ; ;   ;;;;;     ;     ;   ;
;    ;   ;  ;    ;     ;     ;   ;
;    ;   ;  ;   ;;     ;     ;   ;
;   ;;; ;;;  ;;; ;;  ;;;;;  ;;; ;;;
;
;
;
;

;; ---------------------------------------------------------------------------------------------------

;; start the game
(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
            (on-key interact-with-board)
            (to-draw draw-dice-world)
            (stop-when no-more-moves-in-world? draw-end-of-dice-world)))

;;  -> DiceWorld
;; Returns a randomly generated world. If the world that
;; has been generated starts as a tie, the world is regenerated.
;; property: world is not in endgame state (no-more-moves? returns false)
(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER))
  (define new-world (dice-world #f gamet))
  (if (no-more-moves-in-world? new-world) (create-world-of-dice-and-doom) new-world))

;; DiceWorld Key -> DiceWorld
;; Handles key events from a player
(define (interact-with-board w k)
  (cond
    [(key=? "left" k) (refocus-board w left)]
    [(key=? "right" k) (refocus-board w right)]
    [(key=? "p" k) (pass w)]
    [(key=? "\r" k) (mark w)]
    [(key=? "d" k) (unmark w)]
    [else w]))

;; Diceworld -> Scene
;; draws the world
(define (draw-dice-world w)
  #;(printf "in draw: player ~s\n" (game-player (dice-world-gt w)))
  (add-player-info (game-player (dice-world-gt w)) (add-board-to-scene w (ISCENE))))

;; DiceWorld -> Boolean
;; is it possible to play any moves from this world state?
(define (no-more-moves-in-world? w)
  (define tree (dice-world-gt w))
  (define board (dice-world-board w))
  (define player (game-player tree))
  (or (no-more-moves? tree)
      (for/and ([t board])
        (= (territory-player t) player))))

;; DiceWorld -> Image
;; render the endgame screen
(define (draw-end-of-dice-world w)
  (define board (dice-world-board w))
  (define message (text (won board) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene w (PLAIN)))
  (overlay message background))

;; Board -> String
;; Which player has won the game -- eager is for N human players
(define (won board)
  (define-values (best-score w) (winners board))
  (if (cons? (rest w)) "It's a tie." "You won."))

;
;
;
;
;    ;;;;;             ;
;      ;                     ;
;      ;    ;; ;;    ;;;    ;;;;;
;      ;     ;;  ;     ;     ;
;      ;     ;   ;     ;     ;
;      ;     ;   ;     ;     ;
;      ;     ;   ;     ;     ;   ;
;    ;;;;;  ;;; ;;;  ;;;;;    ;;;
;
;
;
;

;; ---------------------------------------------------------------------------------------------------
;; Making A Board

;; -> Board
;; Creates a list of territories the size of GRID with given x and y coordinates
;; properties: dice is (0,MAX-DICE]
;;             returns list of size GRID
(define (territory-build)
  (for/list ([n (in-range GRID)])
    (territory n (modulo n PLAYER#) (dice) (get-x n) (get-y n))))

;; -> Natural
;; the number of initial die on a territory
(define (dice)
  (add1 (random DICE#)))

;; Natural -> Number
;; the x coordinate for territory n of a board
(define (get-x n)
  (+ OFFSET0 (if (odd? (get-row n)) 0 (/ X-OFFSET 2)) (* X-OFFSET (modulo n BOARD))))

;; Natural -> Number
;; the y coordinate for territory n of a board
(define (get-y n)
  (+ OFFSET0 (* Y-OFFSET (get-row n))))

;; ---------------------------------------------------------------------------------------------------
;; Making a Game Tree

;; Board Player Natural -> GameTree
;; creates a complete game-tree from the given board, player, and spare dice
(define (game-tree board player)
  ;; create tree of attacks from this position; add passing move

  ;; -- START: --
  (game board player (attacks board player)))

;; Player -> Player
;; switches from one player to the next
(define (switch player)
  (modulo (+ player 1) PLAYER#))

;; Board Player Natural -> Natural Board
;; adds reinforcements to the game board
;; > (add-new-dice (list (territory 0 2 2 9 0)) 2 2))
;; (list (territory 0 2 2 9 0))
(define (distribute board player spare-dice)
  (for/fold ([dice spare-dice]
             [new-board '()])
            ([t board])
    (if (and (= (territory-player t) player) (< (territory-dice t) DICE#) (not (zero? dice)))
        (values (- dice 1) (cons (add-dice-to t) new-board))
        (values dice (cons t new-board)))))

;; Territory -> Territory
;; adds one dice to the given territory
(define (add-dice-to t)
  (territory-set-dice t (add1 (territory-dice t))))

;; Board Player Territory Natural -> Boolean
;; can player attack dst from src?
(define (attackable? board player src dst)
  (define dst-t (findf (lambda (t) (= (territory-index t) dst)) board))
  (and dst-t
       (= (territory-player src) player)
       (not (= (territory-player dst-t) player))
       (> (territory-dice src) (territory-dice dst-t))))

(define (random-dice dice)
  (for/sum ([_ (in-range dice)]) (add1 (random 6))))

;; Board Natural Natural Natural Natural -> Board
;; Creates a new board after an attack
;; updates only src and dst
(define (execute board player src dst)
  (define src-territory (findf (lambda (a) (= src (territory-index a))) board))
  (define dst-territory (findf (lambda (a) (= dst (territory-index a))) board))
  (define src-dice (territory-dice src-territory))
  (define dst-dice (territory-dice dst-territory))
  (log-debug "src-dice ~s, dst-dice ~s\n" src-dice dst-dice)
  (cond
    [(> (random-dice src-dice) (random-dice dst-dice))
     (log-debug "more\n")
     (set! src-territory (territory-set-dice src-territory 1))
     (define s (territory-set-dice dst-territory (- src-dice 1)))
     (set! dst-territory (territory-set-player s player))]
    [else
     (log-debug "less\n")
     (set! src-territory (territory-set-dice src-territory 1))])
  (log-debug "src-dice ~s, dst-dice ~s\n" (territory-dice src-territory) (territory-dice dst-territory))
  (for/list ([t board])
    (define idx (territory-index t))
    (cond
      [(= idx src) src-territory]
      [(= idx dst) dst-territory]
      [else t])))

;; ---------------------------------------------------------------------------------------------------
;; Getting Neigbors

;; Natural -> [List-of Natural]
;; returns the neighbors of the current spot
;; > (neighbors 0)
;; '(1 2 3)
(define (neighbors pos)
  (define top? (< pos BOARD))
  (define bottom? (= (get-row pos) (sub1 BOARD)))
  (define even-row? (zero? (modulo (get-row pos) 2)))
  (define right? (zero? (modulo (add1 pos) BOARD)))
  (define left? (zero? (modulo pos BOARD)))
  (if even-row? (even-row pos top? bottom? right? left?) (odd-row pos top? bottom? right? left?)))

;; Natural Boolean Boolean Boolean Boolean -> [Listof Naturals]
;; gets the neighbors for a territory on an even row
(define (even-row pos top? bottom? right? left?)
  (append (add (or top? right?) (add1 (- pos BOARD)))
          (add (or bottom? right?) (add1 (+ pos BOARD)))
          (add top? (- pos BOARD))
          (add bottom? (+ pos BOARD))
          (add right? (add1 pos))
          (add left? (sub1 pos))))

;; Natural Boolean Boolean Boolean Boolean -> [Listof Naturals]
;; gets the neighbors for a territory on an odd row
(define (odd-row pos top? bottom? right? left?)
  (append (add top? (- pos BOARD))
          (add bottom? (+ pos BOARD))
          (add (or top? left?) (sub1 (- pos BOARD)))
          (add (or bottom? left?) (sub1 (+ pos BOARD)))
          (add right? (add1 pos))
          (add left? (sub1 pos))))

;; Boolean X -> [Listof X]
;; returns (list x) if (not b) else empty
(define (add b x)
  (if b '() (list x)))

;
;
;
;
;   ;;; ;;;                 ;;;;;;
;    ;   ;                   ;   ;                           ;
;    ;  ;    ;;;;   ;;; ;;;  ; ;   ;;;  ;;;  ;;;;   ;; ;;   ;;;;;    ;;;;;
;    ; ;    ;    ;   ;   ;   ;;;    ;    ;  ;    ;   ;;  ;   ;      ;    ;
;    ;;;    ;;;;;;   ;   ;   ; ;     ;  ;   ;;;;;;   ;   ;   ;       ;;;;
;    ;  ;   ;         ; ;    ;       ;  ;   ;        ;   ;   ;           ;
;    ;   ;  ;         ; ;    ;   ;    ;;    ;        ;   ;   ;   ;  ;    ;
;   ;;;  ;;  ;;;;;     ;    ;;;;;;    ;;     ;;;;;  ;;; ;;;   ;;;   ;;;;;
;                      ;
;                    ;;;
;
;

;; ---------------------------------------------------------------------------------------------------
;; Territory Focusing and Marking

;; DiceWorld [Board -> Board] -> World
;; Creates a new World that has a rotated territory list
;; > (define lterritory (territory 0 0 1 9 2))
;; > (define rterritory (territory 0 0 1 9 0))
;; > (refocus-board-action (dice-world -1 (list rterritory lterritory ...) GT) left)
;; (dice-world -1 (list lterritory ... rterritory) GT)
;; > (refocus-board-action (dice-world -1 (list lterritory ... rterritory) GT) left)
;; (dice-world -1 (list rterritory lterritory ...) GT)
(define (refocus-board w direction)
  (define source (dice-world-src w))
  (define board (dice-world-board w))
  (define tree (dice-world-gt w))
  (define player (game-player tree))
  (define (owner? tid)
    (if source (not (= tid player)) (= tid player)))
  (define new-board (rotate-until owner? board direction))
  
  (dice-world source (game new-board player (game-moves tree))))

;; [Player -> Boolean] Board (Board -> Board) -> Board
;; rotate until the first element of the list satisfies owned-by
(define (rotate-until owned-by board rotate)
  (define next-list (rotate board))
  (if (owned-by (territory-player (first next-list)))
      next-list
      (rotate-until owned-by next-list rotate)))

;; Board -> Board
;; rotate a list to the left
(define (left l)
  (append (rest l) (list (first l))))

;; Board -> Board
;; rotate a list to the right
(define (right l)
  (reverse (left (reverse l))))

;; ---------------------------------------------------------------------------------------------------
;; Handling Moves

;; DiceWorld -> DiceWorld
;; executes a passing move on the world state
;; THIS DEFINITION IS NOT USED FOR THE ABSTRACT VERSION OF THE MODULE.
(define (pass.10 w)
  (define m (find-move (game-moves (dice-world-gt w)) '()))
  (cond
    [(not m) w]
    ;; (no-more-moves? m)
    [else (dice-world #f (game-board m) m)]))

;; DiceWorld -> DiceWorld
;; unmarks a marked territory
(define (unmark w)
  (dice-world #f (dice-world-gt w)))

;; DiceWorld -> DiceWorld
;; marks a territory as the launching pad for an attack or launches the attack
(define (mark w)
  (define tree (dice-world-gt w))
  (define board (dice-world-board w))
  (define source (dice-world-src w))
  (define focus (territory-index (first board)))
  (if source (attacking w source focus) (dice-world focus tree)))

(define (attacks board player)
  (for*/list ([src board]
              [dst (neighbors (territory-index src))]
              #:when (attackable? board player src dst))
    (define from (territory-index src))
    (move (list from dst) '())))

;; DiceWorld Natural Natural -> DiceWorld
(define (attacking w source target)
  (define nextgt (attacking-gt (dice-world-gt w) source target))
  (if nextgt (dice-world #f nextgt) w))

(define (attacking-gt gt source target)
  (define feasible (game-moves gt))
  (define attack (list source target))
  (define nextgt (find-move feasible attack))
  (cond
    [nextgt
     (define board (game-board gt))
     (define src-territory (findf (lambda (a) (= source (territory-index a))) board))
     (define player (territory-player src-territory))
     (define newb (execute board player source target))
     (game newb player (cons (move '() '()) (attacks newb player)))]
    [else #f]))

;; [List-of Moves] [or '() [List Natural Natural]] -> [or #f Game-tree]
;; find the move from the current list of moves
(define (find-move moves a)
  (define m (findf (lambda (m) (equal? (move-action m) a)) moves))
  (and m (move-gt m)))

;; Game -> Boolean
;; are there any moves in this game record?
(define (no-more-moves? g)
  (empty? (game-moves g)))

;
;
;
;
;   ;;;;;                       ;;                     ;
;    ;   ;                       ;
;    ;   ;   ;;;;   ;; ;;    ;;; ;   ;;;;   ;; ;;;   ;;;    ;; ;;    ;;; ;;
;    ;   ;  ;    ;   ;;  ;  ;   ;;  ;    ;   ;;        ;     ;;  ;  ;   ;;
;    ;;;;   ;;;;;;   ;   ;  ;    ;  ;;;;;;   ;         ;     ;   ;  ;    ;
;    ;  ;   ;        ;   ;  ;    ;  ;        ;         ;     ;   ;  ;    ;
;    ;   ;  ;        ;   ;  ;   ;;  ;        ;         ;     ;   ;  ;   ;;
;   ;;;   ;  ;;;;;  ;;; ;;;  ;;; ;;  ;;;;;  ;;;;;    ;;;;;  ;;; ;;;  ;;; ;
;                                                                        ;
;                                                                    ;;;;
;
;

;; Player Scene-> Scene
;; Draws the world
(define (add-player-info player s)
  (define str (whose-turn player))
  (define txt (text str TEXT-SIZE TEXT-COLOR))
  (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET s))

;; DiceWorld Scene -> Scene
;; folds through the board and creates an image representation of it
(define (add-board-to-scene w s)
  (define board (dice-world-board w))
  (define player (game-player (dice-world-gt w)))
  (define focus? (dice-world-src w))
  (define trtry1 (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image (draw-focus focus? p-focus player t-image))
  (define base-s (add-territory trtry1 image s))
  (for/fold ([s base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) s)))

;; Nat Player Player Image -> Image
;; add focus marker to territory if needed
(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p)) (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      t-image))

;; Image Territory Image -> Image
(define (add-territory t image scene)
  (place-image image (territory-x t) (territory-y t) scene))

;; Territory -> Image
;; renders a single territory
(define (draw-territory t)
  (define color (color-chooser (territory-player t)))
  (overlay (hexagon color) (draw-dice (territory-dice t))))

;; Natural -> Image
;; renders all n >= 1 dice as a stack of dice
(define (draw-dice n)
  (define first-die (get-dice-image 0))
  (define height-die (image-height first-die))
  (for/fold ([s first-die]) ([i (- n 1)])
    (define dice-image (get-dice-image (+ i 1)))
    (define y-offset (* height-die (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))

;; Player -> Color
;; Determines a color for each player
(define (color-chooser p)
  (list-ref COLORS p))

;; -> Image
;; returns an image from the list of dice images
(define (get-dice-image i)
  (list-ref IMG-LIST (modulo i (length IMG-LIST))))

;
;
;
;
;   ;;;;;;              ;;     ;
;    ;   ;               ;
;    ; ;    ;; ;;    ;;; ;   ;;;    ;; ;;    ;;; ;;
;    ;;;     ;;  ;  ;   ;;     ;     ;;  ;  ;   ;;
;    ; ;     ;   ;  ;    ;     ;     ;   ;  ;    ;
;    ;       ;   ;  ;    ;     ;     ;   ;  ;    ;
;    ;   ;   ;   ;  ;   ;;     ;     ;   ;  ;   ;;
;   ;;;;;;  ;;; ;;;  ;;; ;;  ;;;;;  ;;; ;;;  ;;; ;
;                                                ;
;                                            ;;;;
;
;

;; Board ->* Natural [non-empty-listof Player]
;; gives the number of winning territories and the players(s) who have them
;; > (winners (list (territory 0 0 1 9 0) (territory 0 0 1 9 1)))
;; (values 2 '(0))
;; > (winners (list (territory 0 1 1 9 0) (territory 0 0 1 9 1)))
;; (values 1 '(0 1))
(define (winners board)
  (for/fold ([best 0]
             [winners '()])
            ([p PLAYER#])
    (define p-score (sum-territory board p))
    (cond
      [(> p-score best) (values p-score (list p))]
      [(< p-score best) (values best winners)]
      [(= p-score best) (values best (cons p winners))])))

;; Board Player -> Natural
;; counts the number of territorys the player owns
;; > (sum-territory (list (territory 0 1 1 9 0) (territory 0 1 1 9 1)) 1)
;; 2
(define (sum-territory board player)
  (for/fold ([result 0]) ([t board])
    (if (= (territory-player t) player) (+ result 1) result)))

;
;
;
;
;
;     ;;;     ;;;;;;;
;      ;;        ;
;     ;  ;       ;
;     ;  ;       ;
;     ;  ;       ;
;    ;;;;;;      ;
;    ;    ;      ;
;   ;      ;     ;
;  ;;;    ;;; ;;;;;;;
;
;
;
;

;; Player -> {AI-TURN, YOUR-TURN}
;; THIS REQUIRES A DIFFERENT DEFINITION FOR PLAIN CHAPTER 10.
(define (whose-turn player)
  (if (= player AI) AI-TURN YOUR-TURN))

(define left-dice INIT-SPARE-DICE)
;; DiceWorld -> DiceWorld
;; executes a passing move on the world state
;; THIS REQUIRES A DIFFERENT DEFINITION FOR PLAIN CHAPTER 10.

(define (passes tree)
  (define player (game-player tree))
  (log-debug "left dice before: ~s\n" left-dice)
  (define-values (new-dice newb) (distribute (game-board tree) player left-dice))
  (set! left-dice new-dice)
  (log-debug "left dice after: ~s\n" left-dice)
  (game-tree newb (switch player)))

(define (pass w)
  (define m (find-move (game-moves (dice-world-gt w)) '()))
  (cond
    [(not m) w]
    [else
     ;'()
     (define nextgt (passes (dice-world-gt w)))
     (cond
       [(or (empty? (game-moves nextgt)) (not (= (game-player nextgt) AI)))
        (dice-world #f nextgt)]
       [else
        (define ai (the-ai-plays nextgt AI))
        (dice-world #f ai)])]))

;; GameTree -> GameTree
;; Computer calls this function until it is no longer the player
(define (the-ai-plays tree ai-player)
  (let loop ([gt tree])
    (define player (game-player gt))
    (define moves (game-moves gt))
    (cond
      [(or (not (= player ai-player)) (empty? moves)) gt]
      [else
       (define sample-move (first (random-sample moves 1)))
       (define action (move-action sample-move)) ;(if (empty? action))
       (cond
         [(empty? action) (passes gt)]
         [else
          (define source (first action))
          (define target (second action))
          (log-debug "ai:============\n")
          (log-debug "gt: ~s\n" gt)
          (log-debug "dice: ~s\n" DICE#)
          (log-debug "attack: ~s ~s\n" source target)
          (loop (attacking-gt gt source target))])])))

;;(define ratings  (rate-moves tree AI-DEPTH))
;;(define the-move (first (argmax second ratings)))
;;(define new-tree (move-gt the-move))
;;(if (= (game-player new-tree) AI)
;;    (the-ai-plays new-tree)
;;    new-tree))

;; GameTree Natural -> [List-of (List Move Number)]
;; assigns a value to each move that is being considered
;; and return those values in a list
(define (rate-moves tree depth)
  (for/list ([move (game-moves tree)])
    (list move (rate-position (move-gt move) (- depth 1)))))

;; GameTree Natural -> Number
;; Returns a number that is the best move for the given player.
(define (rate-position tree depth)
  (cond
    [(or (= depth 0) (no-more-moves? tree))
     (define-values (best w) (winners (game-board tree)))
     (if (member AI w) (/ 1 (length w)) 0)]
    [else
     (define ratings (rate-moves tree depth))
     (apply (if (= (game-player tree) AI) max min) (map second ratings))]))

;
;
;
;
;     ;;
;      ;
;     ; ;   ;;  ;;  ;;  ;;   ;;;;;
;     ; ;    ;   ;   ;  ;   ;    ;
;     ; ;    ;   ;    ;;     ;;;;
;     ;;;    ;   ;    ;;         ;
;    ;   ;   ;  ;;   ;  ;   ;    ;
;   ;;; ;;;   ;; ;; ;;  ;;  ;;;;;
;
;
;
;

;; Natural -> Natural
;; gets the row that territory is on, indexed from 0
;; [test vary on current board-size]
(define (get-row pos)
  (quotient pos BOARD))

(define (log-debug . s)
  (void #;(apply printf s)))
#;(random-seed 10)
#;(roll-the-dice)


(module+ test
  (require rackunit
           rackunit/text-ui)

  (define (simulate)
    (define w (create-world-of-dice-and-doom))
    (define gt (dice-world-gt w))

    (let loop ([i 0])
      (cond
        [(not (no-more-moves-in-world? (dice-world #f gt)))
         (define player (game-player gt))
         (set! gt (the-ai-plays gt player))
         (loop (add1 i))]
        [else i])))

  (for ([i (range 100)])
    (random-seed i)
    (check-true (> (simulate) 0)))
  

  #;(check-equal? 1 0))
