#lang racket

(provide launch-chat-server)

(require 2htdp/universe)

(define (launch-chat-server)
  (universe (hash) (state #t) (on-new connect) (on-msg handle-msg)))

(define (connect u client)
  (define name (iworld-name client))
  (cond [(hash-has-key? u name) (make-bundle u (list (make-mail client #f)) (list client))]
        [else (define new-u (hash-set u (iworld-name client) client))
              (make-bundle new-u empty empty)]))

(define (handle-msg u client msg)
  (make-bundle u
               (for/list ([i (in-hash-values u)])
                 (make-mail i msg))
               '()))