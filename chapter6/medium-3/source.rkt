#lang racket

#|
   The Snake game
   --------------

   The Snake game revolves around a room filled with pieces of radioactive goo
   and a snake that can remove this goo.

   When the snake eats the goo, it grows and new goo appears. Like all
   radioactive material, goo decays over time. Eventually it expires, but
   fortunately for the snake, a new piece of goo appears elsewhere.

   The player is in control of a snake, and the objective is to grow the snake as
   large as possible. She may change the direction of the snake by pressing one of
   the four arrow keys. When the snake gets close to a piece of goo, it eats the
   goo and grows a new segment. If the snake runs into itself or one of the four
   walls, the game is over. The length of the snake is the player's score.

   Play
   ----

   Run and evaluate
     (start-snake)
   This will pop up a window with instructions for interacting with the program.
|#

;
;
;
;
;                                 ;;
;     ;;; ;                        ;
;    ;   ;;                        ;
;    ;    ;   ;; ;;;      ;;;;     ;  ;;;     ;;;
;    ;         ;;   ;    ;    ;    ;  ;      ;   ;
;     ;;;;     ;    ;         ;    ; ;      ;     ;
;         ;    ;    ;    ;;;;;;    ;;;      ;;;;;;;
;    ;    ;    ;    ;   ;     ;    ;  ;     ;
;    ;;   ;    ;    ;   ;    ;;    ;   ;     ;    ;
;    ; ;;;    ;;;  ;;;   ;;;; ;;  ;;  ;;;;    ;;;;
;
;
;
;

(require 2htdp/image
         2htdp/universe)

(require racket/random)
;; -----------------------------------------------------------------------------
;; Data Definitions

;; A Pit is a (pit Snake (Listof Goo))
(struct pit (snake goos obstacles ticks) #:transparent)

;; A Snake is a (make-snake Direction (cons Seg [Listof Seg]))
(struct snake (dir segs) #:transparent)
;; The head of the snake is the first element in the list of segs.
;; Each segment of a snake is located with:
;;  - x in (0,SIZE),
;;  - y in (0,SIZE).
;; And is SEG-SIZE aligned (x and y are multiples of SEG-SIZE).

;; A Seg is a (posn Number Number)

;; A Goo is a (goo Posn Number)
(struct goo (loc expire) #:transparent)
;; The expire field is a Natural Number that represents the number
;; of ticks until the goo expires. A goo is expired when this field is 1

;;Obstacle
(struct obstacle (loc) #:transparent)

;; A Direction is one of "up" "down" "left" "right"

;; A Posn is (posn number number)
(struct posn (x y) #:transparent)
;; Represents a two dimensional point.

;; -----------------------------------------------------------------------------
;; Constants

;; Tick Rate
(define TICK-RATE 1/10)

;; Board Size Constants
(define SIZE 30)

;; Snake Constants
(define SEG-SIZE 15)

;; Goo Constants
(define MAX-GOO 5)
(define EXPIRATION-TIME 150)

;;obstacle Constants
(define OBSTACLE-NUMBER 2)

(define OBSTACLE-MOVE-SEC 1)

;; GRAPHICAL BOARD
(define WIDTH-PX (* SEG-SIZE SIZE))
(define HEIGHT-PX (* SEG-SIZE SIZE))

;; Visual constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))
(define OBSTACLE-IMG (bitmap "graphics/obstacle.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

;
;
;
;                          ;
;                          ;
;  ;;;   ;;;
;   ;;   ;;
;   ; ; ; ;     ;;;;     ;;;      ;; ;;;
;   ; ; ; ;    ;    ;      ;       ;;   ;
;   ; ; ; ;         ;      ;       ;    ;
;   ;  ;  ;    ;;;;;;      ;       ;    ;
;   ;     ;   ;     ;      ;       ;    ;
;   ;     ;   ;    ;;      ;       ;    ;
;  ;;;   ;;;   ;;;; ;;  ;;;;;;;   ;;;  ;;;
;
;
;
;
;; -----------------------------------------------------------------------------

;; Start the Game
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (for/list ([_ (in-range MAX-GOO)])
                   (fresh-goo))
                 (for/list ([_ (in-range OBSTACLE-NUMBER)])
                   (create-obstable))
                 0)
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

;; Pit -> Pit
;; Take one step: eat or slither
(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (define ticks (pit-ticks w))
  (define obstacles
    (if (= (modulo ticks (/ OBSTACLE-MOVE-SEC TICK-RATE)) 0)
        (move-obstacles (pit-obstacles w))
        (pit-obstacles w)))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)) obstacles (add1 ticks))
      (pit (slither snake) (age-goo goos) obstacles (add1 ticks))))

(define (move-obstacles obstacles)
  (map move-obstacle obstacles))

(define (move-obstacle ob)
  (define is-x (random-ref '(#t #f)))
  (define delta (random-ref '(-1 1)))
  (define x (posn-x (obstacle-loc ob)))
  (define y (posn-y (obstacle-loc ob)))
  (if is-x
      (obstacle (posn (clip (+ x delta) 1 (sub1 SIZE)) y))
      (obstacle (posn x (clip (+ y delta) 1 (sub1 SIZE))))))

(define (clip n bottom top)
  (min top (max n bottom)))

;; Pit KeyEvent -> Pit
;; Handle a key event
(define (direct-snake w ke)
  (cond
    [(dir? ke) (world-change-dir w ke)]
    [else w]))

;; Pit -> Scene
;; Render the world as a scene
(define (render-pit w)
  (obstacle+scene (pit-obstacles w) 
               (snake+scene (pit-snake w) (goo-list+scene (pit-goos w) MT-SCENE))))

;; Pit -> Boolean
;; Is the snake dead?
(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake) (obstacle-colliding? snake w)))

;; Pit -> Scene
;; produces a gameover image
(define (render-end w)
  (overlay (text "Game over" ENDGAME-TEXT-SIZE "black") (render-pit w)))

;
;
;
;     ;;;;    ;;                    ;;              ;;;;;;     ;            ;;
;    ;   ;     ;                     ;              ;  ;                     ;
;   ;          ;     ;;;;    ;;; ;   ; ;;;;            ;     ;;;     ;;; ;   ; ;;;;  ;;;;;
;   ;          ;    ;    ;  ;;  ;;   ;  ;              ;       ;    ;;  ;;   ;  ;   ;    ;
;   ;          ;    ;    ;  ;        ;;;               ;       ;    ;        ;;;     ;;;;
;   ;          ;    ;    ;  ;        ; ;               ;       ;    ;        ; ;         ;
;    ;   ;     ;    ;    ;  ;    ;   ;  ;              ;       ;    ;    ;   ;  ;   ;    ;
;     ;;;    ;;;;;   ;;;;    ;;;;   ;;  ;;;           ;;;    ;;;;;   ;;;;   ;;  ;;; ;;;;;
;
;
;
;

;; -----------------------------------------------------------------------------
;; Movement and Eating

;; -----------------------------------------------------------------------------
;; Eating and Growth

;; Snake [Listof Goo] -> Goo or #f
;; Can the snake eat any of the goos?
;; > (can-eat (snake "right" `(,(posn 3 3))) `(,(goo (posn 3 3) 130)))
;; (goo (posn 3 3) 130)
(define (can-eat snake goos)
  (cond
    [(empty? goos) #f]
    [else (if (close? (snake-head snake) (first goos)) (first goos) (can-eat snake (rest goos)))]))

;; [Listof Goo] Goo -> [Listof Goo]
;; Eat and replenish a goo.
;; > (eat (list (goo (posn 5 5) 5)) (goo (posn 5 5) 5))
;; (list (new-goo))
(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

;; Seg Goo -> Boolean
;; Is the segment close to the goo?
;; > (close? (posn 1 2) (goo (posn 1 2) 4))
;; #t
(define (close? s g)
  (posn=? s (goo-loc g)))

;; Grow the snake one segment.
;; Snake -> Snake
;; > (grow snake0)
;; (snake "right" `(,(posn 2 1) ,(posn 1 1)))
(define (grow sn)
  (snake (snake-dir sn) (cons (next-head sn) (snake-segs sn))))

;; -----------------------------------------------------------------------------
;; Movement

;; Snake -> Snake
;; Slither the snake forward one segment.
;; > (slither snake0)
;; (snake "right" (posn 2 1))
(define (slither sn)
  (snake (snake-dir sn) (cons (next-head sn) (all-but-last (snake-segs sn)))))

;; Snake -> Seg
;; Compute the next head position of the snake.
;; > (next-head snake0)
;; (snake "right" (list (posn 2 1)))
(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond
    [(string=? dir "up") (posn-move head 0 -1)]
    [(string=? dir "down") (posn-move head 0 1)]
    [(string=? dir "left") (posn-move head -1 0)]
    [(string=? dir "right") (posn-move head 1 0)]))

;; Posn Number Number -> Posn
;; Move the position by dx, dy.
;; > (posn-move (posn 1 1) 2 3)
;; (posn 3 4)
(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx) (+ (posn-y p) dy)))

;; (cons X [Listof X]) -> [Listof X]
;; Returns a List that is does not contain the last element of the given list.
;; > (all-but-last '(1 2 3 4))
;; '(1 2 3)
(define (all-but-last segs)
  (cond
    [(empty? (rest segs)) empty]
    [else (cons (first segs) (all-but-last (rest segs)))]))

;; -----------------------------------------------------------------------------
;; Rotting Goo

;; [Listof Goo] -> [Listof Goo]
;; Renew and rot goos.
(define (age-goo goos)
  (rot (renew goos)))

;; [Listof Goo] -> [Listof Goo]
;; Renew any rotten goos.
(define (renew goos)
  (cond
    [(empty? goos) empty]
    [(rotten? (first goos)) (cons (fresh-goo) (renew (rest goos)))]
    [else (cons (first goos) (renew (rest goos)))]))

;; [Listof Goo] -> [Listof Goo]
;; Rot all of the goos.
(define (rot goos)
  (cond
    [(empty? goos) empty]
    [else (cons (decay (first goos)) (rot (rest goos)))]))

;; Goo -> Boolean
;; has the goo expired?
;; > (rotten? (goo 1 2) 0)
;; #t
(define (rotten? g)
  (zero? (goo-expire g)))

;; Goo -> Goo
;; decreases the expire field of goo by one
;; > (decay (goo (posn 1 2) 2))
;; (goo (posn 1 2) 1)
(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))

;; -> Goo
;; Create random goo with fresh expiration.
;; Property: The position of the goo is:
;;  - x in (0,WIDTH),
;;  - y in (0,HEIGHT).
(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE))) (add1 (random (sub1 SIZE)))) EXPIRATION-TIME))

;; ->Obstacle
;; create random obstacle
(define (create-obstable)
  (obstacle (posn (add1 (random (sub1 SIZE))) (add1 (random (sub1 SIZE))))))

;
;
;
;
;
;   ;;; ;;;;
;    ;   ;
;    ;  ;       ;;;    ;;;   ;;;   ;;;; ;
;    ; ;       ;   ;    ;     ;   ;    ;;
;    ;;;;     ;     ;    ;   ;    ;
;    ;   ;    ;;;;;;;    ;   ;     ;;;;;
;    ;   ;    ;           ; ;           ;
;    ;    ;    ;    ;     ; ;     ;     ;
;   ;;;   ;;    ;;;;       ;      ;;;;;;
;                          ;
;                         ;
;                      ;;;;;
;
;; -----------------------------------------------------------------------------

;; String -> Boolean
;; Is the given value a direction?
;; > (dir? "up")
;; #t
(define (dir? x)
  (or (string=? x "up") (string=? x "down") (string=? x "left") (string=? x "right")))

;; Pit Direction-> Pit
;; Change the direction (if not opposite current snake dir)
;; > (world-change-dir world0 "up")
;; (pit snake1 (list goo0))
(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond
    [(and (opposite-dir? (snake-dir the-snake) d)
          ;; consists of the head and at least one segment:
          (cons? (rest (snake-segs the-snake))))
     (stop-with w)]
    [else (pit (snake-change-dir the-snake d) (pit-goos w) (pit-obstacles w) (pit-ticks w))]))

;; Direction Direction -> Boolean
;; Are d1 and d2 opposites?
;; > (opposite-dir? "up" "down")
;; #t
(define (opposite-dir? d1 d2)
  (cond
    [(string=? d1 "up") (string=? d2 "down")]
    [(string=? d1 "down") (string=? d2 "up")]
    [(string=? d1 "left") (string=? d2 "right")]
    [(string=? d1 "right") (string=? d2 "left")]))

;
;
;
;
;                                      ;;
;   ;;;;;;                              ;
;    ;    ;                             ;
;    ;    ;     ;;;     ;; ;;;      ;;; ;     ;;;      ;;  ;;;
;    ;    ;    ;   ;     ;;   ;    ;   ;;    ;   ;      ;;;
;    ;;;;;    ;     ;    ;    ;   ;     ;   ;     ;     ;
;    ;  ;     ;;;;;;;    ;    ;   ;     ;   ;;;;;;;     ;
;    ;   ;    ;          ;    ;   ;     ;   ;           ;
;    ;    ;    ;    ;    ;    ;    ;   ;;    ;    ;     ;
;   ;;;   ;;    ;;;;    ;;;  ;;;    ;;; ;;    ;;;;     ;;;;;
;
;
;
;
;; -----------------------------------------------------------------------------

;; Snake Scene -> Scene
;; Draws the snake onto the scene
;; > (snake+scene snake0 MT-SCENE)
;; (place-image SEG-IMG 8 8 MT-SCENE)
(define (snake+scene snake scene)
  (define snake-body-scene (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond
               [(string=? "up" dir) HEAD-UP-IMG]
               [(string=? "down" dir) HEAD-DOWN-IMG]
               [(string=? "left" dir) HEAD-LEFT-IMG]
               [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

(define (obstacle+scene obstacles scene)
  (define (get-posns-from-obstacle obstacles)
    (cond
      [(empty? obstacles) empty]
      [else (cons (obstacle-loc (first obstacles)) (get-posns-from-obstacle (rest obstacles)))]))
  (img-list+scene (get-posns-from-obstacle obstacles) OBSTACLE-IMG scene))
;; [Listof Goo] Scene -> Scene
;; draws all of the goo to a scene
;; > (goo-list+scene (list goo0) MT-SCENE)
;; (place-image GOO-IMG 32 32 MT-SCENE)
(define (goo-list+scene goos scene)
  ;; [Listof Goo] -> [Listof Posn]
  ;; gets the posns of all the goo
  ;; > (get-posns-from-goo (list (goo (posn 2 2) 1) (goo (posn 3 3) 1))
  ;; (list (posn 2 2) (posn 3 3))
  (define (get-posns-from-goo goos)
    (cond
      [(empty? goos) empty]
      [else (cons (goo-loc (first goos)) (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

;; [Listof Posn] Image Scene -> Scene
;; Draws a the image to each posn in the list
;; > (img-list+scene (list (posn 1 1)) GOO-IMG MT-SCENE)
;; (place-image GOO-IMG 8 8
;;              (img-list+scene empty GOO-IMG MT-SCENE))
(define (img-list+scene posns img scene)
  (cond
    [(empty? posns) scene]
    [else (img+scene (first posns) img (img-list+scene (rest posns) img scene))]))

;; Posn Image Scene -> Scene
;; Draws a the given image onto the scene at the posn.
;; > (img+scene (posn 2 2) GOO-IMG MT-SCENE)
;; (place-image GOO-IMG 32 32 MT-SCENE)
(define (img+scene posn img scene)
  (place-image img (* (posn-x posn) SEG-SIZE) (* (posn-y posn) SEG-SIZE) scene))

;
;
;
;
;                            ;;
;   ;;;;;;;                   ;               ;;;; ;
;    ;    ;                   ;              ;    ;;
;    ;    ;   ;; ;;;      ;;; ;             ;           ;;;;   ;; ;  ;      ;;;
;    ;  ;      ;;   ;    ;   ;;             ;          ;    ;   ;; ;; ;    ;   ;
;    ;;;;      ;    ;   ;     ;             ;               ;   ;  ;  ;   ;     ;
;    ;  ;      ;    ;   ;     ;             ;   ;;;;;  ;;;;;;   ;  ;  ;   ;;;;;;;
;    ;    ;    ;    ;   ;     ;             ;      ;  ;     ;   ;  ;  ;   ;
;    ;    ;    ;    ;    ;   ;;              ;     ;  ;    ;;   ;  ;  ;    ;    ;
;   ;;;;;;;   ;;;  ;;;    ;;; ;;              ;;;;;    ;;;; ;; ;;; ;; ;;    ;;;;
;
;
;
;
;; -----------------------------------------------------------------------------

;; Snake -> Boolean
;; Determine if the snake is colliding with itself.
;; > (self-colliding? (snake "up" (list (posn 1 1) (posn 2 1)
;;                                      (posn 2 2) (posn 1 2)
;;                                      (posn 1 1))))
;; #t
(define (self-colliding? sn)
  (cons? (member (snake-head sn) (snake-body sn))))

;; Snake -> Boolean
;; Determine if the snake is colliding with any of the walls.
;; > (wall-colliding? (snake "up" (list (posn 0 1))))
;; #t
(define (wall-colliding? sn)
  (define x (posn-x (snake-head sn)))
  (define y (posn-y (snake-head sn)))
  (or (= 0 x) (= x SIZE) (= 0 y) (= y SIZE)))

(define (obstacle-colliding? sn w)
  (for/or ([s (in-list (snake-segs sn))])
    (define x (posn-x s))
    (define y (posn-y s))
    (define obstacles (pit-obstacles w))
    (for/or ([o (in-list obstacles)])
      (define loc (obstacle-loc o))
      (define ox (posn-x loc))
      (define oy (posn-y loc))
      (and (= x ox) (= y oy)))))

;
;
;
;
;
;     ;;;     ;;;  ;;; ;;;   ;;;  ;;;;;;;   ;;;;;     ;;;;;;;     ;;;     ;;;;;;   ;;;   ;;;
;      ;;      ;    ;   ;     ;      ;        ;          ;         ;;      ;    ;   ;     ;
;     ;  ;     ;    ;    ;   ;       ;        ;          ;        ;  ;     ;    ;    ;   ;
;     ;  ;     ;    ;     ; ;        ;        ;          ;        ;  ;     ;    ;     ; ;
;     ;  ;     ;    ;      ;         ;        ;          ;        ;  ;     ;;;;;       ;
;    ;;;;;;    ;    ;     ; ;        ;        ;    ;     ;       ;;;;;;    ;  ;        ;
;    ;    ;    ;    ;    ;   ;       ;        ;    ;     ;       ;    ;    ;   ;       ;
;   ;      ;   ;    ;   ;     ;      ;        ;    ;     ;      ;      ;   ;    ;      ;
;  ;;;    ;;;   ;;;;   ;;;   ;;;  ;;;;;;;   ;;;;;;;;  ;;;;;;;  ;;;    ;;; ;;;   ;;   ;;;;;
;
;
;
;
;; -----------------------------------------------------------------------------
;; Posn Posn -> Boolean
;; Are the two posns are equal?
;; > (posn=? (posn 1 1) (posn 1 1))
;; true
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2)) (= (posn-y p1) (posn-y p2))))

;; Access the head position of the snake.
;; snake-head : Snake -> Seg
;; > (snake-head (snake "right" (list (posn 1 1) (posn 2 1)))
;; (posn 1 1)
(define (snake-head sn)
  (first (snake-segs sn)))

;; Snake -> [Listof Segs]
;; returns the snake's body.
;; That is everyting that isn't the snake's head.
(define (snake-body sn)
  (rest (snake-segs sn)))

;; Snake Direction -> Snake
(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))
