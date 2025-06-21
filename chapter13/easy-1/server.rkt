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
