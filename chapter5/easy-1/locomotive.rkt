#lang racket

(require 2htdp/universe 2htdp/image)

(define WIDTH 200)
(define HEIGHT 300)

;; depending on your settings, drracket may claim that the image causes a syntax error
;; please ignore this warning and click RUN anyway

(define IMAGE-of-LOCO (bitmap/file "locomotive.png"))

(define (add-3-to-state current-state)
  (if (> current-state (+ WIDTH (/ (image-width IMAGE-of-LOCO) 2)))
      (- 0 (/ (image-width IMAGE-of-LOCO) 2)) 
      (+ current-state 3)))

(define (draw-a-locomotive-onto-an-empty-scene current-state)
  (place-image IMAGE-of-LOCO current-state (/ HEIGHT 2) 
               (empty-scene WIDTH HEIGHT)))

(define (never-stop current-state)
  #f)

(define (main)
  (big-bang 0                                ;; initial state 
    (on-tick add-3-to-state)                 ;; when the clock ticks, add ...
    (to-draw draw-a-locomotive-onto-an-empty-scene) ;; when the state changes, draw ... 
    (stop-when never-stop)))         ;; never stop. 

;; to run the program, click run and enter 
;;   > (main) 
;; at the prompt.

