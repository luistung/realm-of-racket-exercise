#lang racket

(require 2htdp/image
         2htdp/universe)

(provide launch-chat-client)

(struct message (from body) #:transparent)

(define (message->sexp m)
  (list 'message (message-from m) (message-body m)))

(define (sexp->message s)
  (unless (eq? (car s) 'message)
    (error (format "~a not struct message\n")))
  (message (second s) (third s)))

(struct world ([name #:mutable] [messages #:mutable] [input #:mutable]) #:transparent)

(define WIDTH 150)
(define DISPLAY-HEIGHT 140)
(define INPUT-HEIGHT 10)

(define (draw w)

  (define name (world-name w))
  (define input (world-input w))
  (define messages (world-messages w))
  (define messages-text
    (string-join (map (lambda (m) (format "~a:~a" (message-from m) (message-body m))) messages) "\n"))
  (overlay/align "middle"
                 "bottom"
                 (overlay/align "left"
                                "middle"
                                (text (format "~a:~a" name input) INPUT-HEIGHT "red")
                                (empty-scene WIDTH INPUT-HEIGHT))
                 (overlay/align "middle"
                                "top"
                                (overlay/align "left"
                                               "top"
                                               (text messages-text INPUT-HEIGHT "blue")
                                               (empty-scene WIDTH DISPLAY-HEIGHT "gray"))
                                (empty-scene WIDTH (+ DISPLAY-HEIGHT INPUT-HEIGHT)))))

(define (handle-keys w key)

  (define (alpha-beta key)
    (define ascii
      (findf (lambda (x) (key=? key (string (integer->char x))))
             (range (char->integer #\a) (add1 (char->integer #\z)))))
    (and ascii (string (integer->char ascii))))

  (define prefix (world-input w))
  (cond
    [(alpha-beta key)
     =>
     (lambda (x)
       (set-world-input! w (string-append prefix x))
       w)]
    [(key=? key "\r")
     (set-world-input! w "")
     (make-package w (message->sexp (message (world-name w) prefix)))]
    [(key=? key "\b")
     (set-world-input! w (substring prefix 0 (sub1 (string-length prefix))))
     w]
    [else w]))

(define (handle-msg w msg)
  (cond
    [(not msg) #f]
    [else
     (define messages (world-messages w))
     (set-world-messages! w (append messages (list (sexp->message msg))))
     w]))

(define last-picture
  (overlay/align "left"
                 "top"
                 (text "disconnected by server" INPUT-HEIGHT "blue")
                 (empty-scene WIDTH (+ DISPLAY-HEIGHT INPUT-HEIGHT))))

(define (launch-chat-client n host)
  (big-bang (world n (list) "")
            (on-draw draw)
            (on-key handle-keys)
            (name n)
            (register host)
            (on-receive handle-msg)
            (stop-when false? (lambda (x) last-picture))))