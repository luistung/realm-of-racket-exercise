Change our version of Dice of Doom so that the dice are rolled for an attack.  Have both the attacker and defender roll the number of dice on their respective territories.  If the attacker wins, she takes the territory. If the defender wins, the attacker loses all but one dice on her territory. You may want to get the random numbers from a third-party site, such as www.random.org. Here’s some code for retrieving a bunch of random numbers:
(define (get-random-numbers)
 (define n 1000)
 (define src
   (string-append
    "http://www.random.org/integers/?num="
    (number->string n)
    "&min=1&max=6&col=5&base=10&format=plain&rnd=new"))
 (define ip (get-pure-port (string->url src)))
 ;; -- go get them --
 (for/list ((i n)) (read ip)))
