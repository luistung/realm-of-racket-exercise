#lang racket

;; This module implements the server for the Hungry Henry game

(provide bon-appetit ;; -> Void
         ;; launch the server for Hungry Henry
         )

(require "shared.rkt"
         2htdp/universe)

#| -----------------------------------------------------------------------------
The server is responsible for:
-- starting the game
-- moving Henrys
-- have Henrys eat, remove food on collision
-- collecting and broadcasting information about the movement of players
-- ending games
|#

;
;
;
;   ;   ;                                            ;   ;
;   ;   ;                                            ;   ;
;   ;   ;  ;   ;  ; ;;    ;; ;  ; ;;;  ;   ;         ;   ;   ;;;   ; ;;   ; ;;;  ;   ;
;   ;   ;  ;   ;  ;;  ;  ;  ;;  ;;  ;  ;   ;         ;   ;  ;   ;  ;;  ;  ;;  ;  ;   ;
;   ;;;;;  ;   ;  ;   ;  ;   ;  ;       ;  ;         ;;;;;  ;   ;  ;   ;  ;       ;  ;
;   ;   ;  ;   ;  ;   ;  ;   ;  ;       ; ;          ;   ;  ;;;;;  ;   ;  ;       ; ;
;   ;   ;  ;   ;  ;   ;  ;   ;  ;       ; ;          ;   ;  ;      ;   ;  ;       ; ;
;   ;   ;  ;  ;;  ;   ;  ;  ;;  ;        ;           ;   ;  ;      ;   ;  ;        ;
;   ;   ;   ;; ;  ;   ;   ;; ;  ;        ;           ;   ;   ;;;;  ;   ;  ;        ;
;                            ;           ;                                         ;
;                         ;;;          ;;                                        ;;
;

;; Init Constants
(define TICK .1)
(define PLAYER-LIMIT 2)
(define START-TIME 0)
(define WAIT-TIME 250)
(define ADD-FOOD# 0)
(define food-remaider 0)

(define FOOD*PLAYERS 5)

(define WEIGHT-FACTOR 2.1)
(define BASE-SPEED (/ (expt PLAYER-SIZE 2) WEIGHT-FACTOR))

;; Data Definitions
(struct join (clients [time #:mutable]) #:transparent)
(struct play (players food spectators) #:transparent #:mutable)

;; plus some update primitives:

;; JoinUniverse Player -> JoinUniverse
(define (join-add-player j new-p)
  (join (cons new-p (join-clients j)) (join-time j)))

;; PlayUniverse IP -> PlayUniverse
(define (play-add-spectator pu new-s)
  (define players (play-players pu))
  (define spectators (play-spectators pu))
  (play players (play-food pu) (cons new-s spectators)))

;; PlayUniverse IWorld -> PlayUniverse
;; removes player that uses iworld
(define (play-remove p iw)
  (define players (play-players p))
  (define spectators (play-spectators p))
  (play (rip iw players) (play-food p) (rip iw spectators)))

;; JoinUniverse IWorld -> JoinUniverse
;; removes players and spectators that use iw from this world
(define (join-remove j iw)
  (join (rip iw (join-clients j)) (join-time j)))

;; IWorld [Listof Player] -> [Listof Player]
;; remove player that contains the given IWorld
(define (rip iw players)
  (remove iw players (lambda (iw p) (iworld=? iw (ip-iw p)))))

;; LIKE:
;; (struct ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
(define-values (ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
  (let ()
    (struct ip (id iw body waypoints player) #:transparent)
    (define (create iw id body waypoints)
      (ip id iw body waypoints (player id body waypoints)))
    (values create ip? ip-id ip-iw ip-body ip-waypoints ip-player)))

;; ServerState is one of
;; -- JoinUniverse
;; -- PlayUniverse
;; JoinUniververse = (join [Listof IPs] Nat)
;; interpretation:
;; -- the first field lists the currently connected client-player
;; -- the second field is the number of ticks since the server started
;; PlayUniverse    = (play [Listof IPs] [Listof Food] [Listof IP])
;; interpretation:
;; -- the first field lists all participating players
;; -- the second field lists the cupcakes
;; --- the third field enumerates the spectating players
;; IP              = (ip Id IWorld Body [Listof Complex] Feaster)
;; interpretation:
;; the struct represents the Universe's perspective of a connected player
;; -- the first field is the assigned unique Id
;; -- the second field is the IWorld representing the remote connection to the client
;; -- the third field is the Body of the player
;; -- the fourth field is the list of player-chosen Waypoints,
;;     ordered from oldest click to most-recent
;;     meaning the first one has to be visited first by the Henry
;; -- the fifth field is the serialized representation of the first four fields

(define JOIN0 (join empty START-TIME))

;
;
;
;
;   ;;; ;;;            ;
;    ;; ;;
;    ;; ;;   ;;;;    ;;;    ;; ;;
;    ; ; ;  ;    ;     ;     ;;  ;
;    ; ; ;   ;;;;;     ;     ;   ;
;    ;   ;  ;    ;     ;     ;   ;
;    ;   ;  ;   ;;     ;     ;   ;
;   ;;; ;;;  ;;; ;;  ;;;;;  ;;; ;;;
;
;
;
;

(define (bon-appetit)
  (universe JOIN0
            (on-new connect)
            (on-msg handle-goto-message)
            (on-tick tick-tock TICK)
            (on-disconnect disconnect)))

;; ServerState IWorld -> Bundle
;; adds a new connection to a JoinUniverse and ticks. Ignores otherwise
(define (connect s iw)
  (cond
    [(join? s) (add-player s iw)]
    [(play? s) (add-spectator s iw)]))

;; ServerState IWorld Sexpr -> Bundle
;; accepts goto messages from clients
(define (handle-goto-message s iw msg)
  (cond
    [(and (play? s) (goto? msg)) (goto s iw msg)]
    [else (empty-bundle s)]))

;; ServerState -> Bundle
;; handle a tick event
(define (tick-tock s)
  (cond
    [(join? s) (wait-or-play s)]
    [(play? s) (move-and-eat s)]))

;; ServerState IWorld -> Bundle
;; handles loss of a client
(define (disconnect s iw)
  (cond
    [(join? s) (drop-client s iw)]
    [(play? s) (drop-player s iw)]))

;
;
;
;  ;     ;          ;             ;
;  ;     ;                ;
;  ;     ; ;;;;   ;;;    ;;;;;  ;;;    ; ;;    ;; ;
;  ;  ;  ;     ;    ;     ;       ;    ;;  ;  ;  ;;
;  ;  ;  ;     ;    ;     ;       ;    ;   ;  ;   ;
;   ;; ;;   ;;;;    ;     ;       ;    ;   ;  ;   ;
;   ;; ;;  ;   ;    ;     ;       ;    ;   ;  ;   ;
;   ;   ;  ;  ;;    ;     ;       ;    ;   ;  ;  ;;
;   ;   ;   ;;  ;   ;      ;;;    ;    ;   ;   ;; ;
;                                                 ;
;                                              ;;;
;

;; JoinUniverse -> Bundle
;; count down and might transition
(define (wait-or-play j)
  (cond
    [(keep-waiting? j) (keep-waiting j)]
    [else (start-game j)]))

;; JoinUniverse -> Boolean
;; is it time to start?
(define (keep-waiting? j)
  (or (> PLAYER-LIMIT (length (join-clients j))) (> WAIT-TIME (join-time j))))

;; JoinUniverse -> [Bundle JoinUniverse]
(define (keep-waiting j)
  (set-join-time! j (+ (join-time j) 1))
  (time-broadcast j))

;; JoinUniverse -> [Bundle JoinUniverse]
;; broadcasts the new load time fraction to the players
(define (time-broadcast j)
  (define iworlds (map ip-iw (join-clients j)))
  (define load% (min 1 (/ (join-time j) WAIT-TIME)))
  (make-bundle j (broadcast iworlds load%) empty))

;; JoinUniverse -> [Bundle PlayUniverse]
;; starts the game
(define (start-game j)
  (define clients (join-clients j))
  (define cupcakes (bake-cupcakes (length clients)))
  (broadcast-universe (play clients cupcakes empty)))

;; Number -> [Listof Food]
;; creates the amount of food for that number of players
(define (bake-cupcakes player#)
  (for/list ([i (in-range (* player# FOOD*PLAYERS))])
    (create-a-body CUPCAKE)))

;
;
;          ;;;
;   ;;;;     ;                    ;
;   ;   ;    ;
;   ;   ;    ;    ;;;;   ;   ;  ;;;    ; ;;    ;; ;
;   ;  ;     ;        ;  ;   ;    ;    ;;  ;  ;  ;;
;   ;;;      ;        ;   ;  ;    ;    ;   ;  ;   ;
;   ;        ;     ;;;;   ; ;     ;    ;   ;  ;   ;
;   ;        ;    ;   ;   ; ;     ;    ;   ;  ;   ;
;   ;        ;    ;  ;;    ;      ;    ;   ;  ;  ;;
;   ;        ;     ;;  ;   ;      ;    ;   ;   ;; ;
;                          ;                      ;
;                        ;;                    ;;;
;

;; PlayUniverse -> Bundle
;; moves everything. eats. may end game
(define (move-and-eat pu)
  (define nplayers (move-player* (play-players pu)))
  (define nfood (feed-em-all nplayers (play-food pu)))
  (progress nplayers nfood (play-spectators pu)))

;; [Listof IP] -> [Listof IP]
;; moves all players
(define (move-player* players)
  (for/list ([p players])
    (define waypoints (ip-waypoints p))
    (cond
      [(empty? waypoints) p]
      [else
       (define body (ip-body p))
       (define nwpts (move-toward-waypoint body waypoints))
       (ip (ip-iw p) (ip-id p) body nwpts)])))

;; Body [Listof Complex] -> [Listof Complex]
;; effect: set body's location
;; determine new waypoints for player
;; pre: (cons? waypoints)
(define (move-toward-waypoint body waypoints)
  (define goal (first waypoints))
  (define bloc (body-loc body))
  (define line (- goal bloc))
  (define dist (magnitude line))
  (define speed (/ BASE-SPEED (body-size body)))
  (cond
    [(<= dist speed)
     (set-body-loc! body goal)
     (rest waypoints)]
    [else ; (> distance speed 0)
     (set-body-loc! body (+ bloc (* (/ line dist) speed)))
     waypoints]))

;; [Listof Player] [Listof Food] -> [Listof Food]
;; feeds all players and removes food
(define (feed-em-all players foods)
  (define origin-food# (length foods))
  (define new-foods
    (for/fold ([foods foods]) ([p players])
      (eat-all-the-things p foods)))
  (define-values (quotient remainder)
    (quotient/remainder (+ food-remaider (- origin-food# (length new-foods))) 2))
  (set! food-remaider remainder)
  (for/fold ([new-foods new-foods]) ([i (* quotient ADD-FOOD#)])
    (cons (create-a-body CUPCAKE) new-foods)))

;; IP [Listof Food] -> [Listof Food]
;; effect: fatten player as he eats
;; determine left-over foods
(define (eat-all-the-things player foods)
  (define b (ip-body player))
  (for/fold ([foods '()]) ([f foods])
    (cond
      [(body-collide? f b)
       (set-body-size! b (+ PLAYER-FATTEN-DELTA (body-size b)))
       foods]
      [else (cons f foods)])))

;; body body -> Boolean
;; Have two bodys collided?
(define (body-collide? s1 s2)
  (<= (magnitude (- (body-loc s1) (body-loc s2))) (+ (body-size s1) (body-size s2))))

;; [Listof Ip] [Listof Food] [Listof IP] -> Bundle
;; moves all objects. may end game
(define (progress pls foods spectators)
  (define p (play pls foods spectators))
  (cond
    [(empty? foods) (end-game-broadcast p)]
    [else (broadcast-universe p)]))

;; PlayUniverse -> [Bundle JoinUniverse]
;; ends the game, and restarts it
(define (end-game-broadcast p)
  (define iws (get-iws p))
  (define msg (list SCORE (score (play-players p))))
  (define mls (broadcast iws msg))
  (make-bundle (remake-join p) mls empty))

;; Play-Universe -> JoinUniverse
;; Readies the ServerState for a new game
(define (remake-join p)
  (define players (refresh (play-players p)))
  (define spectators (play-spectators p))
  (join (append players spectators) START-TIME))

;; [Listof Players] -> [Listof Players]
;; creates new players for new game
(define (refresh players)
  (for/list ([p players])
    (create-player (ip-iw p) (ip-id p))))

;; [Listof IP] -> [Listof (list Id Score)]
;; makes the endgame message informing clients of all the size
(define (score ps)
  (for/list ([p ps])
    (list (ip-id p) (get-score (body-size (ip-body p))))))

;
;
;
;
;   ;;; ;;;
;    ;; ;;
;    ;; ;;   ;;;;    ;;;;;   ;;;;;   ;;;;    ;;; ;;  ;;;;    ;;;;;
;    ; ; ;  ;    ;  ;    ;  ;    ;  ;    ;  ;   ;;  ;    ;  ;    ;
;    ; ; ;  ;;;;;;   ;;;;    ;;;;    ;;;;;  ;    ;  ;;;;;;   ;;;;
;    ;   ;  ;            ;       ;  ;    ;  ;    ;  ;            ;
;    ;   ;  ;       ;    ;  ;    ;  ;   ;;  ;   ;;  ;       ;    ;
;   ;;; ;;;  ;;;;;  ;;;;;   ;;;;;    ;;; ;;  ;;; ;   ;;;;;  ;;;;;
;                                                ;
;                                            ;;;;
;
;

;; -----------------------------------------------------------------------------
;; Play Universe

;; Message -> Boolean
;; checks if message is a drag
(define (goto? msg)
  (and (list? msg)
       (= GOTO-LENGTH (length msg))
       (symbol? (first msg))
       (number? (second msg))
       (number? (third msg))
       (symbol=? GOTO (first msg))
       (<= 0 (second msg) WIDTH)
       (<= 0 (third msg) HEIGHT)))

;; PlayUniverse IWorld GotoMessage -> PlayUniverse
;; handles a player clicking. checks for collisions, updates score, removes food
;; Effect: changes a player's waypoints
(define (goto p iw msg)
  (define c (make-rectangular (second msg) (third msg)))
  (set-play-players! p (add-waypoint (play-players p) c iw))
  (broadcast-universe p))

;; [Listof IPs] Complex IWorld -> [Listof IPs]
;; adds that complex to the waypoints of the given players
(define (add-waypoint ps c iw)
  (for/list ([p ps])
    (cond
      [(iworld=? (ip-iw p) iw)
       (ip (ip-iw p) (ip-id p) (ip-body p) (append (ip-waypoints p) (list c)))]
      [else p])))

;
;
;
;
;     ;;;;                                                     ;
;    ;   ;                                           ;
;   ;        ;;;;   ;; ;;   ;; ;;    ;;;;    ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;
;   ;       ;    ;   ;;  ;   ;;  ;  ;    ;  ;   ;;   ;         ;    ;    ;   ;;  ;
;   ;       ;    ;   ;   ;   ;   ;  ;;;;;;  ;        ;         ;    ;    ;   ;   ;
;   ;       ;    ;   ;   ;   ;   ;  ;       ;        ;         ;    ;    ;   ;   ;
;    ;   ;  ;    ;   ;   ;   ;   ;  ;       ;    ;   ;   ;     ;    ;    ;   ;   ;
;     ;;;    ;;;;   ;;; ;;; ;;; ;;;  ;;;;;   ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;;
;
;
;
;

;; -----------------------------------------------------------------------------
;; Join Universe

;; [Universe Player -> Universe] -> [Universe IWorld -> [Bundle Universe]]
;; creates a function that deals with a new connection during join or play phase
(define (make-connection adder)
  (lambda (u iw)
    (define player (named-player iw))
    (define mails (list (make-mail iw (ip-id player))))
    (make-bundle (adder u player) mails empty)))

;; JoinUniverse IWorld ID -> [Bundle JoinUniverse]
;; creates an internal player for the IWorld, adds it to Universe as waiting player
(define add-player (make-connection join-add-player))

;; PlayUniverse IWorld -> [Bundle PlayUniverse]
;; creates an internal player for the IWorld, adds it to Universe as spectator
(define add-spectator (make-connection play-add-spectator))

;; [Listof IP] IWorld ->* Player
(define (named-player iw)
  (create-player iw (symbol->string (gensym (iworld-name iw)))))

;
;
;
;
;    ;;; ;                     ;              ;;       ;
;   ;   ;;                                     ;
;   ;        ;;;;   ;; ;;;   ;;;     ;;;;      ;     ;;;     ;;;;;   ;;;;
;    ;;;;   ;    ;   ;;        ;    ;    ;     ;       ;     ;  ;   ;    ;
;        ;  ;;;;;;   ;         ;     ;;;;;     ;       ;       ;    ;;;;;;
;        ;  ;        ;         ;    ;    ;     ;       ;      ;     ;
;   ;;   ;  ;        ;         ;    ;   ;;     ;       ;     ;   ;  ;
;   ; ;;;    ;;;;;  ;;;;;    ;;;;;   ;;; ;;  ;;;;;   ;;;;;   ;;;;;   ;;;;;
;
;
;
;

;; PlayUniverse -> [Bundle PlayUniverse [Listof [Mail StateMessage]]]
;; bundle this universe, serialize it, broadcast it, and drop noone
(define (broadcast-universe p)
  (define mails (broadcast (get-iws p) (serialize-universe p)))
  (make-bundle p mails empty))

;; [Listof IWorlds] Message -> [Listof Mail]
;; sends mail to all clients
(define (broadcast iws msgs)
  (map (lambda (iw) (make-mail iw msgs)) iws))

;; PlayUniverse -> (list s [Listof SerializedPlayer] [Listof SerializedFood])
;; prepairs a message for an update world/ServerState state
(define (serialize-universe p)
  (define serialized-players (map ip-player (play-players p)))
  (list SERIALIZE serialized-players (play-food p)))

;
;
;
;
;   ;;;;       ;
;    ;  ;                                                                    ;
;    ;   ;   ;;;     ;;;;;   ;;; ;   ;;;;   ;; ;;   ;; ;;    ;;;;    ;;; ;  ;;;;;
;    ;   ;     ;    ;    ;  ;   ;;  ;    ;   ;;  ;   ;;  ;  ;    ;  ;   ;;   ;
;    ;   ;     ;     ;;;;   ;       ;    ;   ;   ;   ;   ;  ;;;;;;  ;        ;
;    ;   ;     ;         ;  ;       ;    ;   ;   ;   ;   ;  ;       ;        ;
;    ;  ;      ;    ;    ;  ;    ;  ;    ;   ;   ;   ;   ;  ;       ;    ;   ;   ;
;   ;;;;     ;;;;;  ;;;;;    ;;;;    ;;;;   ;;; ;;; ;;; ;;;  ;;;;;   ;;;;     ;;;
;
;
;
;

;; JoinUniverse IWorld -> Bundle
;; remove that iworld from list of clients
(define (drop-client j iw)
  (empty-bundle (join-remove j iw)))

;; PlayUniverse IWorld -> Bundle
;; removes a player from the ServerState and tells the players
(define (drop-player p iw)
  (broadcast-universe (play-remove p iw)))

;
;
;
;
;     ;;
;      ;
;     ; ;   ;;  ;;  ;;  ;;
;     ; ;    ;   ;   ;  ;
;     ; ;    ;   ;    ;;
;     ;;;    ;   ;    ;;
;    ;   ;   ;  ;;   ;  ;
;   ;;; ;;;   ;; ;; ;;  ;;
;
;
;
;

;; Number -> Body
;; creates a random body, that does not touch the edge
(define (create-a-body size)
  (define x (+ size (random (- WIDTH size))))
  (define y (+ size (random (- HEIGHT size))))
  (body size (make-rectangular x y)))

;; PlayUniverse -> [Listof IWorlds]
;; gets the iworlds of all players
(define (get-iws p)
  (map ip-iw (append (play-players p) (play-spectators p))))

;; ServerState -> Bundle
;; makes a bundle that sends no messages and disconnects noone
(define (empty-bundle s)
  (make-bundle s empty empty))

;; IWorld Id -> IP
;; creates a player with that idnumber
(define (create-player iw n)
  (ip iw n (create-a-body PLAYER-SIZE) empty))