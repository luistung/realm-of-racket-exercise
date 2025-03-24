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
;; -----------------------------------------------------------------------------
;; Data Definitions

;; A Pit is a (pit Snake (Listof Goo))
(struct pit (snake-1 snake-2 goos result) #:transparent)

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

;; GRAPHICAL BOARD
(define WIDTH-PX (* SEG-SIZE SIZE))
(define HEIGHT-PX (* SEG-SIZE SIZE))

;; Visual constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-1-IMG (bitmap "graphics/body-1.gif"))
(define HEAD-1-IMG (bitmap "graphics/head-1.gif"))
(define SEG-2-IMG (bitmap "graphics/body-2.gif"))
(define HEAD-2-IMG (bitmap "graphics/head-2.gif"))

(define HEAD-1-LEFT-IMG HEAD-1-IMG)
(define HEAD-1-DOWN-IMG (rotate 90 HEAD-1-LEFT-IMG))
(define HEAD-1-RIGHT-IMG (flip-horizontal HEAD-1-LEFT-IMG))
(define HEAD-1-UP-IMG (flip-vertical HEAD-1-DOWN-IMG))

(define HEAD-2-LEFT-IMG HEAD-2-IMG)
(define HEAD-2-DOWN-IMG (rotate 90 HEAD-2-LEFT-IMG))
(define HEAD-2-RIGHT-IMG (flip-horizontal HEAD-2-LEFT-IMG))
(define HEAD-2-UP-IMG (flip-vertical HEAD-2-DOWN-IMG))

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
                 (snake "left" (list (posn (sub1 SIZE) (sub1 SIZE))))
                 (for/list ([_ (in-range MAX-GOO)])
                   (fresh-goo))
                 #f)
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when (lambda _ #f) render-end)))

;; Pit -> Pit
;; Take one step: eat or slither
(define (next-pit w)
  (define snake-1 (pit-snake-1 w))
  (define snake-2 (pit-snake-2 w))
  (define goos (pit-goos w))
  (define winner (who-is-win w))

  (cond
    [winner (stop-with (pit snake-1 snake-2 goos winner))]
    [else
     (define goo-to-eat-1 (can-eat snake-1 goos))
     (define goo-to-eat-2 (can-eat snake-2 goos))

     (cond
       [goo-to-eat-1
        (set! goos (eat goos goo-to-eat-1))
        (set! snake-1 (grow snake-1))]
       [else (set! snake-1 (slither snake-1))])
     (cond
       [goo-to-eat-2
        (set! goos (eat goos goo-to-eat-2))
        (set! snake-2 (grow snake-2))]
       [else (set! snake-2 (slither snake-2))])
     (pit snake-1 snake-2 (age-goo goos) #f)]))

;; Pit KeyEvent -> Pit
;; Handle a key event
(define (direct-snake w ke)
  (define ke-1 (dir-1? ke))
  (define ke-2 (dir-2? ke))
  (cond
    [ke-1
     (define new-snake-1 (world-change-dir (pit-snake-1 w) ke-1))
     (if (not new-snake-1)
         (stop-with (pit (pit-snake-1 w) (pit-snake-2 w) (pit-goos w) 2))
         (pit new-snake-1 (pit-snake-2 w) (pit-goos w) #f))]
    [ke-2
     (define new-snake-2 (world-change-dir (pit-snake-2 w) ke-2))
     (if (not new-snake-2)
         (stop-with (pit (pit-snake-1 w) (pit-snake-2 w) (pit-goos w) 1))
         (pit (pit-snake-1 w) new-snake-2 (pit-goos w) #f))]
    [else w]))

;; Pit -> Scene
;; Render the world as a scene
(define (render-pit w)
  (snake+scene (pit-snake-2 w)
               #f
               (snake+scene (pit-snake-1 w) #t (goo-list+scene (pit-goos w) MT-SCENE))))

;; Pit -> Boolean
;; Is the snake dead?
(define (dead? snake w)
  (or (self-colliding? snake) (wall-colliding? snake)))

(define (same-pos? a b)
  (and (= (posn-x a) (posn-x b)) (= (posn-y a) (posn-y b))))

(define (bite? head body)
  (for/or ([b (in-list body)])
    (same-pos? head b)))

(define (who-is-win w)
  (define snake-1 (pit-snake-1 w))
  (define snake-2 (pit-snake-2 w))
  (define snake-1-dead (dead? snake-1 w))
  (define snake-2-dead (dead? snake-2 w))
  (cond
    [(and snake-1-dead snake-2-dead) 0]
    [snake-1-dead 2]
    [snake-2-dead 1]
    [(same-pos? (snake-head snake-1) (snake-head snake-2)) 0]
    [(bite? (snake-head snake-1) (snake-body snake-2)) 1]
    [(bite? (snake-head snake-2) (snake-body snake-1)) 2]
    [else #f]))

;; Pit -> Scene
;; produces a gameover image
(define (render-end w)
  (overlay (text (case (pit-result w)
                   [(1) "play 1 win"]
                   [(2) "play 2 win"]
                   [else "tie"])
                 ENDGAME-TEXT-SIZE
                 "black")
           (render-pit w)))

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
(define (dir-1? x)
  (cond
    [(string=? x "up") "up"]
    [(string=? x "down") "down"]
    [(string=? x "left") "left"]
    [(string=? x "right") "right"]
    [else #f]))

(define (dir-2? x)
  (cond
    [(string=? x "w") "up"]
    [(string=? x "s") "down"]
    [(string=? x "a") "left"]
    [(string=? x "d") "right"]
    [else #f]))

;; Pit Direction-> Pit
;; Change the direction (if not opposite current snake dir)
;; > (world-change-dir world0 "up")
;; (pit snake1 (list goo0))
(define (world-change-dir the-snake d)
  (cond
    [(and (opposite-dir? (snake-dir the-snake) d)
          ;; consists of the head and at least one segment:
          (cons? (rest (snake-segs the-snake))))
     (displayln "$$$$$$")
     #f]
    [else (snake-change-dir the-snake d)]))

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
(define (snake+scene snake is-first scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) (if is-first SEG-1-IMG SEG-2-IMG) scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond
               [(string=? "up" dir) (if is-first HEAD-1-UP-IMG HEAD-2-UP-IMG)]
               [(string=? "down" dir) (if is-first HEAD-1-DOWN-IMG HEAD-2-DOWN-IMG)]
               [(string=? "left" dir) (if is-first HEAD-1-LEFT-IMG HEAD-2-LEFT-IMG)]
               [(string=? "right" dir) (if is-first HEAD-1-RIGHT-IMG HEAD-2-RIGHT-IMG)])
             snake-body-scene))

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
