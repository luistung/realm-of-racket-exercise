#lang racket

;; the server for distributed Guess my Number

;; starts the distributed guess my number game
;; -> GmNState
(provide launch-guess-server)

(require 2htdp/image
         2htdp/universe
         "shared.rkt")

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

;; A GmNState is one of:
;;  -- #f
;;  -- GuessRange

(struct interval (small big) #:transparent)
;; A GuessRange is (interval Number Number)
;; always true: (interval l u) means (<= l u)

(define u0 (interval LOWER UPPER))

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

(define (launch-guess-server)
  (universe (hash) (state #t) (on-new connect) (on-msg handle-msg)))

;; GmNState IWorld -> [Bundle GmNState [Listof [Mail IWorld Nat]] [Listof IWorld]]
;; handles all new connections. It only accepts one connection.
(define (connect u client)
  (define hash-size (length (hash->list u)))
  (cond
    [(< hash-size 2)
     (define new-u (hash-set u (iworld-name client) client))
     (if (zero? hash-size)
         (make-bundle new-u empty '())
         (make-bundle new-u (list (make-mail (hash-ref new-u "Beatrice") #f)) '()))]
    [else (make-bundle u empty (list client))]))

(define (the-other-client u client)
  (cdr (findf (lambda (x) (not (string=? (car x) (iworld-name client)))) (hash->list u))))

;; GmNState IWorld CtoSMessage -> [Bundle GmNState [List [Mail IWorld Nat]] Empty]
;; handles a message from the client.
(define (handle-msg u client msg)
  (make-bundle u (list (make-mail (the-other-client u client) msg)) '()))

;; GmNState CtoSMessage -> GmNState
;; creates the new universe for a responce
(define (next-interval u msg)
  (cond
    [(not (string? msg)) u]
    [(string=? "up" msg) (bigger u)]
    [(string=? "down" msg) (smaller u)]
    [else u]))

;
;
;
;
;     ;; ;
;    ;  ;;
;   ;       ;;  ;;   ;;;;    ;;;;;   ;;;;;
;   ;        ;   ;  ;    ;  ;    ;  ;    ;
;   ;  ;;;;  ;   ;  ;;;;;;   ;;;;    ;;;;
;   ;    ;   ;   ;  ;            ;       ;
;    ;   ;   ;  ;;  ;       ;    ;  ;    ;
;     ;;;     ;; ;;  ;;;;;  ;;;;;   ;;;;;
;
;
;
;

;; GuessRange -> Boolean
;; Does the interval represent a single number?
;; > (single? (interval 1 1))
;; #t
(define (single? w)
  (= (interval-small w) (interval-big w)))

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
