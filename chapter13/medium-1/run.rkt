#lang racket

(require 2htdp/universe
         "client.rkt"
         "server.rkt")

(define (run)
  (launch-many-worlds (launch-chat-server)
                      (launch-chat-client "Adam" LOCALHOST)
                      (launch-chat-client "Beatrice" LOCALHOST)
                      (launch-chat-client "Beatrice" LOCALHOST)))

#;(run)