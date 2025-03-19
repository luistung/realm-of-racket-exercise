#lang racket

;; as proposed by:
;; https://github.com/racket/realm/pull/6

(require 2htdp/universe
         2htdp/image)

(define WIDTH 200)
(define HEIGHT 300)
(define INIT-SIZE 3)
(define SIZE-DELTA 0.5)
(define INIT-COLOR-TRANSPARENCY 40)

;; depending on your settings, drracket may claim that the image causes a syntax error
;; please ignore this warning and click RUN anyway
(define (half n)
  (/ n 2))
(define IMAGE-of-UFO (bitmap/file "ufo.png"))
(define UFO-WIDTH (image-width IMAGE-of-UFO))
(define UFO-HEIGHT (image-height IMAGE-of-UFO))
(define UP (half UFO-HEIGHT))
(define BOTTOM (- HEIGHT (half UFO-HEIGHT)))
(define LEFT (half UFO-WIDTH))
(define RIGHT (- WIDTH (half UFO-WIDTH)))
(define DELTA 8)
(define MAX-SMOKE 50)
(define TRANSPARENCY-DELTA (round (/ INIT-COLOR-TRANSPARENCY MAX-SMOKE)))

(struct pos (x y) #:transparent)
(struct smoke (x y size trans) #:transparent)
(struct state (p trail) #:transparent)

(define (push-and-keep l e max-size)
  (let loop ([l (cons e l)]
             [n max-size])
    (if (or (empty? l) (zero? n)) empty (cons (first l) (loop (rest l) (sub1 n))))))

(define (move current-state +x +y)
  (define x (pos-x (state-p current-state)))
  (define y (pos-y (state-p current-state)))

  (define nx (max LEFT (min RIGHT (+ x +x))))
  (define ny (max UP (min BOTTOM (+ y +y))))

  (define trail (state-trail current-state))
  (define enlarged-trail
    (map (lambda (a)
           (smoke (smoke-x a)
                  (smoke-y a)
                  (+ (smoke-size a) SIZE-DELTA)
                  (max 0 (- (smoke-trans a) TRANSPARENCY-DELTA))))
         trail))
  (define n-trail
    (push-and-keep enlarged-trail (smoke nx ny INIT-SIZE INIT-COLOR-TRANSPARENCY) MAX-SMOKE))
  (state (pos nx ny) n-trail))

(define (add-3-to-state current-state)
  (move current-state 0 3))

(define (draw-a-ufo-onto-an-empty-scene current-state)

  (for/foldr
   ([acc
     (place-image IMAGE-of-UFO
                  (pos-x (state-p current-state))
                  (pos-y (state-p current-state))
                  (empty-scene WIDTH HEIGHT))])
   ([s (in-list (state-trail current-state))])
   (place-image (circle (smoke-size s) (smoke-trans s) "gray") (smoke-x s) (smoke-y s) acc)))

(define (move-with-key current-state a-key)
  (cond
    [(key=? a-key "left") (move current-state (- DELTA) 0)]
    [(key=? a-key "right") (move current-state DELTA 0)]
    [(key=? a-key "up") (move current-state 0 (- DELTA))]
    [(key=? a-key "down") (move current-state 0 DELTA)]
    [else current-state]))

(define (main)
  (big-bang (state (pos (half WIDTH) 0) (list)) ;; initial state
            (on-key move-with-key) ;;when press key
            (on-tick add-3-to-state) ;; when the clock ticks, add ...
            (to-draw draw-a-ufo-onto-an-empty-scene) ;; when the state changes, draw ...
            (stop-when (lambda _ #f)))) ;; when the UFO lands on Earth, stop.

;; to run the program, click run and enter
;;   > (main)
;; at the prompt.