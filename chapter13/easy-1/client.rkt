#lang racket

;; the client for distributed Guess my Number
(require 2htdp/image
         2htdp/universe
         "shared.rkt")

(provide launch-guess-client1
         launch-guess-client2)

;
;
;
;
;
;   ;;;;;;               ;
;    ;    ;              ;
;    ;     ;    ;;;;    ;;;;;;      ;;;;
;    ;     ;   ;    ;    ;         ;    ;
;    ;     ;        ;    ;              ;
;    ;     ;   ;;;;;;    ;         ;;;;;;
;    ;     ;  ;     ;    ;        ;     ;
;    ;    ;   ;    ;;    ;    ;   ;    ;;
;   ;;;;;;     ;;;; ;;    ;;;;     ;;;; ;;
;
;
;
;

;; ClientState = String
(define ClientState0 "no guess available")

;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define TEXT-SIZE 11)
(define HELP-TEXT (text "↑ for larger numbers, ↓ for smaller ones" TEXT-SIZE "blue"))
(define HELP-TEXT2 (text "Press = when your number is guessed; q to quit." TEXT-SIZE "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define COLOR "red")
(define SIZE 72)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
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

;; String -> ClientState
;; Launch the Client.
(define (launch-guess-client1 n host)
  (big-bang ClientState0
            (on-draw draw-guess)
            (on-key handle-keys)
            (name n)
            (register host)
            (on-receive handle-msg)
            (stop-when false?)))

;; handle-keys: ClientState Key -> [Package ClientState CtoSMessage] or ClientState
;; if the key is "up" or "down", ask the server for a different guess
(define (handle-keys w key)
  (cond
    [(key=? key "up") (make-package w "up")]
    [(key=? key "down") (make-package w "down")]
    [(key=? key "q") (make-package #f "q")]
    [(key=? key "=") (make-package #f "q")]
    [else w]))

;; handle-msg: ClientState StoCMessage -> ClientState
;; if the message is a number, you got a new guess
(define (handle-msg c msg)
  (number->string msg))

;; draw-guess: ClientState -> Scene
;; renders the state as an image
(define (draw-guess c)
  (overlay (text c SIZE COLOR) MT-SC))

;; client2

;; A GmNState is one of:
;;  -- #f
;;  -- GuessRange

(struct interval (small big) #:transparent)
;; A GuessRange is (interval Number Number)
;; always true: (interval l u) means (<= l u)

(define u0 (interval LOWER UPPER))

;; GuessRange -> Number
;; Calculates a guess based on the given interval
;; > (guess (interval 0 100))
;; 50
(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

;; GuessRange -> GuessRange
;; Recreates a GuessRange that lowers the upper bound
;; > (smaller (interval 0 100))
;; (interval 0 50)
(define (smaller w)
  (interval (interval-small w) (max (interval-small w) (sub1 (guess w)))))

;; GuessRange -> GuessRange
;; Recreates a interval that raises the lower bound
;; > (bigger (0 100)
;; (interval 51 100)
(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w))) (interval-big w)))

(define (next-interval u msg)
  (cond
    [(not (string? msg)) u]
    [(string=? "up" msg) (bigger u)]
    [(string=? "down" msg) (smaller u)]
    [else u]))

(define (draw-guess2 c)
  (overlay (text (format "~a" c) SIZE COLOR) MT-SC))

(define (handle-keys2 w key)
  w)

(define (handle-msg2 c msg)
  (cond
    [(and (string? msg) (string=? msg "q")) #f]
    [else
     (define w (if (not msg) u0 (next-interval c msg)))
     (make-package w (guess w))]))

(define (launch-guess-client2 n host)
  (big-bang u0
            (on-draw draw-guess2)
            (on-key handle-keys2)
            (name n)
            (register host)
            (on-receive handle-msg2)
            (stop-when false?)))
