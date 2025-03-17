#lang racket

#|
   The Guess My Number game, with a simple graphical user interface (GUI)
   ----------------------------------------------------------------------

   You pick a number. The program guesses the nunber,
   by asking you questions. Your responses are "too
   small" "too large" or "you guessed it".

   Play
   ----

   Click Run. Pick a number X between <n> and <m>.
   Evaluate
     (start <n> <m>)
   This will pop up a window with instructions for interacting with the program.
|#

(require 2htdp/image
         2htdp/universe)

;
;
;
;                    ;
;
;  ; ;; ;; ;;;;    ;;;   ; ;;
;  ;; ;; ;     ;     ;   ;;  ;
;  ;  ;  ;  ;;;;     ;   ;   ;
;  ;  ;  ; ;   ;     ;   ;   ;
;  ;  ;  ; ;  ;;     ;   ;   ;
;  ;  ;  ;  ;;  ;    ;   ;   ;
;
;

;; Number Number -> GuessRange
;; Start playing a new game in [n,m]
;; > (start 0 100)  ; Press up, up, down, q.
;; (interval 76 87)
(define (start lower upper)
  (big-bang (world (interval lower upper) 1)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render)))

;
;
;
;
;           ;             ;
;    ;;;;  ;;;;;  ;;;;   ;;;;;   ;;;
;   ;       ;         ;   ;     ;   ;
;    ;;     ;      ;;;;   ;     ;;;;;
;      ;    ;     ;   ;   ;     ;
;       ;   ;     ;  ;;   ;     ;
;   ;;;;     ;;;   ;;  ;   ;;;   ;;;;
;
;

(struct interval (small big) #:transparent)
;; A GuessRange is a (interval Number Number)
;; Always true: (interval l u) means (<= l u).

(struct world (interval times) #:transparent)
;
;
;
;
;                                ;                    ;
;    ;;;;   ;;;   ; ;;    ;;;;  ;;;;;  ;;;;   ; ;;   ;;;;;   ;;;;
;   ;      ;   ;  ;;  ;  ;       ;         ;  ;;  ;   ;     ;
;   ;      ;   ;  ;   ;   ;;     ;      ;;;;  ;   ;   ;      ;;
;   ;      ;   ;  ;   ;     ;    ;     ;   ;  ;   ;   ;        ;
;   ;      ;   ;  ;   ;      ;   ;     ;  ;;  ;   ;   ;         ;
;    ;;;;   ;;;   ;   ;  ;;;;     ;;;   ;;  ; ;   ;    ;;;  ;;;;
;
;

(define TEXT-SIZE 11)
(define HELP-TEXT (text "↑ for larger numbers, ↓ for smaller ones" TEXT-SIZE "blue"))
(define HELP-TEXT2 (text "Press = when your number is guessed; q to quit." TEXT-SIZE "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define COLOR "red")
(define TIMES-COLOR "gray")
(define SIZE 72)
(define TIMES-SIZE 24)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define TIMES-X (/ WIDTH 2))
(define TIMES-Y 115)
(define MT-SC
  (place-image/align
   HELP-TEXT
   TEXT-X
   TEXT-UPPER-Y
   "left"
   "top"
   (place-image/align HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom" (empty-scene WIDTH HEIGHT))))

;
;
;   ;                        ;  ;;;
;   ;                        ;    ;
;   ;                        ;    ;
;   ; ;;   ;;;;   ; ;;    ;; ;    ;     ;;;   ; ;;;   ;;;;
;   ;;  ;      ;  ;;  ;  ;  ;;    ;    ;   ;  ;;  ;  ;
;   ;   ;   ;;;;  ;   ;  ;   ;    ;    ;;;;;  ;       ;;
;   ;   ;  ;   ;  ;   ;  ;   ;    ;    ;      ;         ;
;   ;   ;  ;  ;;  ;   ;  ;  ;;    ;    ;      ;          ;
;   ;   ;   ;;  ; ;   ;   ;; ;    ;     ;;;;  ;      ;;;;
;
;

;; GuessRange -> Boolean
;; Does the interval represent a single number?
;; > (single? (interval 1 1))
;; #t
(define (single? w)
  (= (interval-small (world-interval w)) (interval-big (world-interval w))))

;; GuessRange -> Number
;; Calculates a guess based on the given interval
;; > (guess (interval 0 100))
;; 50
(define (guess w)
  (quotient (+ (interval-small (world-interval w)) (interval-big (world-interval w))) 2))

;; GuessRange -> GuessRange
;; Recreates a GuessRange that lowers the upper bound
;; > (smaller (interval 0 100))
;; (interval 0 50)
(define (smaller w)
  (world (interval (interval-small (world-interval w))
                   (max (interval-small (world-interval w)) (sub1 (guess w))))
         (add1 (world-times w))))

;; GuessRange -> GuessRange
;; Recreates a interval that raises the lower bound
;; > (bigger (0 100)
;; (interval 51 100)
(define (bigger w)
  (world (interval (min (interval-big (world-interval w)) (add1 (guess w)))
                   (interval-big (world-interval w)))
         (add1 (world-times w))))

;; GuessRange Key -> GuessRange
;; Handles key input
;; > (key-handler (interval 0 100) "up")
;; (interval 51 100)
;; > (key-handler (interval 0 100) "q")
;; (stop-with (interval 0 100))
(define (deal-with-guess w key)
  (cond
    [(key=? key "up") (bigger w)]
    [(key=? key "down") (smaller w)]
    [(key=? key "q") (stop-with w)]
    [(key=? key "=") (stop-with w)]
    [else w]))

;
;
;                            ;
;                            ;                   ;
;                            ;
;   ; ;;;   ;;;   ; ;;    ;; ;   ;;;   ; ;;;   ;;;   ; ;;    ;; ;
;   ;;  ;  ;   ;  ;;  ;  ;  ;;  ;   ;  ;;  ;     ;   ;;  ;  ;  ;;
;   ;      ;;;;;  ;   ;  ;   ;  ;;;;;  ;         ;   ;   ;  ;   ;
;   ;      ;      ;   ;  ;   ;  ;      ;         ;   ;   ;  ;   ;
;   ;      ;      ;   ;  ;  ;;  ;      ;         ;   ;   ;  ;  ;;
;   ;       ;;;;  ;   ;   ;; ;   ;;;;  ;         ;   ;   ;   ;; ;
;                                                               ;
;                                                           ;;;;

;; GuessRange -> Scene
;; Visualize given interval as a scene
;; > (render (interval 0 100))
;; (overlay (text "50" 72 "red") MT-SC)
(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR)
           (place-image
            (text (string-append "times: " (number->string (world-times w))) TIMES-SIZE TIMES-COLOR)
            TIMES-X
            TIMES-Y
            MT-SC)))

;
;
;
;
;    ;                    ;
;   ;;;;;   ;;;    ;;;;  ;;;;;   ;;;;
;    ;     ;   ;  ;       ;     ;
;    ;     ;;;;;   ;;     ;      ;;
;    ;     ;         ;    ;        ;
;    ;     ;          ;   ;         ;
;     ;;;   ;;;;  ;;;;     ;;;  ;;;;
;
;

(module+ test

  (require rackunit
           rackunit/text-ui)

  ;; testing the 'model' functions for basic guesses

  (check-true (single? (world (interval 50 50) 1)))
  (check-false (single? (world (interval 50 51) 1)))

  (check-equal? (guess (world (interval 0 100) 1)) 50)
  (check-equal? (guess (world (interval 50 100) 1)) 75)
  (check-equal? (guess (world (interval 0 50) 1)) 25)

  (check-equal? (smaller (world (interval 0 100) 1)) (world (interval 0 49) 2))
  (check-equal? (smaller (world (interval 0 000) 1)) (world (interval 0 0) 2))
  (check-equal? (smaller (world (interval 0 50) 1)) (world (interval 0 24) 2))
  (check-equal? (smaller (world (interval 50 100) 1)) (world (interval 50 74) 2))
  (check-equal? (smaller (bigger (bigger (world (interval 0 100) 1)))) (world (interval 76 87) 4))

  (check-equal? (bigger (world (interval 0 100) 1)) (world (interval 51 100) 2))
  (check-equal? (bigger (world (interval 0 000) 1)) (world (interval 0 0) 2))
  (check-equal? (bigger (world (interval 0 100) 1)) (world (interval 51 100) 2))
  (check-equal? (bigger (world (interval 51 100) 1)) (world (interval 76 100) 2))
  (check-equal? (bigger (world (interval 0 50) 1)) (world (interval 26 50) 2))

  (check-equal? (deal-with-guess (world (interval 0 100) 1) "up") (world (interval 51 100) 2))
  (check-equal? (deal-with-guess (world (interval 0 100) 1) "down") (world (interval 0 49) 2))
  (check-equal? (deal-with-guess (world (interval 0 100) 1) "=")
                (stop-with (world (interval 0 100) 1)))
  (check-equal? (deal-with-guess (world (interval 0 100) 1) "q")
                (stop-with (world (interval 0 100) 1)))
  (check-equal? (deal-with-guess (world (interval 0 100) 1) "up") (world (interval 51 100) 2))
  (check-equal? (deal-with-guess (world (interval 50 100) 1) "up") (world (interval 76 100) 2))
  (check-equal? (deal-with-guess (world (interval 0 100) 1) "down") (world (interval 0 49) 2))
  (check-equal? (deal-with-guess (world (interval 0 50) 1) "down") (world (interval 0 24) 2))
  (check-equal? (deal-with-guess (world (interval 50 100) 1) "e") (world (interval 50 100) 1))
  (check-equal? (deal-with-guess (world (interval 0 100) 1) "f") (world (interval 0 100) 1))
  (check-equal? (deal-with-guess (deal-with-guess (world (interval 1 10) 1) "up") "down")
                (world (interval 6 7) 3))

  ;; testing the view functions

  (check-equal?
   (render (world (interval 0 100) 1))
   (overlay
    (text "50" SIZE COLOR)
    (place-image (text (string-append "times: " "1") TIMES-SIZE TIMES-COLOR) TIMES-X TIMES-Y MT-SC)))
  (check-equal?
   (render (world (interval 0 100) 1))
   (overlay
    (text "50" SIZE COLOR)
    (place-image (text (string-append "times: " "1") TIMES-SIZE TIMES-COLOR) TIMES-X TIMES-Y MT-SC)))
  (check-equal?
   (render (world (interval 0 50) 1))
   (overlay
    (text "25" SIZE COLOR)
    (place-image (text (string-append "times: " "1") TIMES-SIZE TIMES-COLOR) TIMES-X TIMES-Y MT-SC)))
  (check-equal?
   (render (world (interval 50 100) 1))
   (overlay
    (text "75" SIZE COLOR)
    (place-image (text (string-append "times: " "1") TIMES-SIZE TIMES-COLOR) TIMES-X TIMES-Y MT-SC)))

  "all tests run")