;; CONSTANTS
(define TILE-LEN 100) ; the length of N-puzzle tile
(define TILE-COLOR 'deepskyblue)
(define BORD-COLOR 'black)
(define TEXT-COLOR 'black)
(define TEXT-SIZE 36)
(define SCENE-LEN (* 3 TILE-LEN)) ; 3 columns
(define e-scene (empty-scene SCENE-LEN SCENE-LEN)) 
(define INITMOVES 15) ; number of moves to create an initial board

;; tval --> image
;; Purpose: Return the tile image for the given tile number
(define (make-tile-img a-tval)
  (if (= a-tval 0)
      (overlay (square TILE-LEN 'outline BORD-COLOR)
               (square TILE-LEN 'solid TILE-COLOR))
      (overlay (text (number->string a-tval) TEXT-SIZE TEXT-COLOR)
               (square TILE-LEN 'outline BORD-COLOR)
               (square TILE-LEN 'solid TILE-COLOR))))

;; Sample expressions for make-tile-img
(define T0 (overlay (square TILE-LEN 'outline BORD-COLOR)
                    (square TILE-LEN 'solid TILE-COLOR)))

(define T1 (overlay (text (number->string 1) TEXT-SIZE TEXT-COLOR)
                    (square TILE-LEN 'outline BORD-COLOR)
                    (square TILE-LEN 'solid TILE-COLOR)))

(define T2 (overlay (text (number->string 2) TEXT-SIZE TEXT-COLOR)
                    (square TILE-LEN 'outline BORD-COLOR)
                    (square TILE-LEN 'solid TILE-COLOR)))

;; Tests using sample computations for make-tile-img
(check-expect (make-tile-img 0) T0)
(check-expect (make-tile-img 1) T1)
(check-expect (make-tile-img 2) T2)

;; Tests using sample values for make-tile-img
(check-expect (make-tile-img 8) .)
(check-expect (make-tile-img 5) .)


(define T3 (make-tile-img 3))

(define T4 (make-tile-img 4))

(define T5 (make-tile-img 5))

(define T6 (make-tile-img 6))

(define T7 (make-tile-img 7))

(define T8 (make-tile-img 8))

;; WORLD DATA DEFINITION

;; A board position (bpos) is an integer in [0..8]

;; Sample bpos
(define TRCRNR 0)
(define CENTER 4)
(define MLEFTC 5)

;; Function template f-on-bpos

#|
;; bpos ... --> ...
;; Purpose:
(define (f-on-bpos a-bpos ...)
  (cond [(= a-bpos 0) ...]
        [(= a-bpos 1) ...]
        [(= a-bpos 2) ...]
        [(= a-bpos 3) ...]
        [(= a-bpos 4) ...]
        [(= a-bpos 5) ...]
        [(= a-bpos 6) ...]
        [(= a-bpos 7) ...]
        [else ...]))

;; Sample expressions for f-on-bpos
(define TRCRNR-VAL ...)
(define CENTER-VAL ...)
(define MLEFTC-VAL ...)

;; Tests using sample computations for f-on-bpos
(check-expect (f-on-bpos TRCRNR ...) TRCRNR-VAL)
(check-expect (f-on-bpos CENTER ...) CENTER-VAL)
(check-expect (f-on-bpos MLEFTC ...) MLEFTC-VAL)

;; Tests using sample values for f-on-bpos
(check-expect (f-on-bpos ... ...) ...)
     ...
|#

;; A tile value, tval, is in [0..8]

;; Sample tval
(define BLNK 0)
(define FOUR 4)
(define FIVE 5)

;; Function template f-on-tval

#|
;; tval ... --> ...
;; Purpose:
(define (f-on-tval a-tval ...)
  (cond [(= a-tval 0) ...]
        [(= a-tval 1) ...]
        [(= a-tval 2) ...]
        [(= a-tval 3) ...]
        [(= a-tval 4) ...]
        [(= a-tval 5) ...]
        [(= a-tval 6) ...]
        [(= a-tval 7) ...]
        [else ...]))

;; Sample expressions for f-on-tval
(define BLNK-VAL ...)
(define FOUR-VAL ...)
(define FIVE-VAL ...)

;; Tests using sample computations for f-on-tval
(check-expect (f-on-tval BLNK ...) BLNK-VAL)
(check-expect (f-on-tval FOUR ...) FOUR-VAL)
(check-expect (f-on-tval FIVE ...) FIVE-VAL)

;; Tests using sample values for f-on-tval
(check-expect (f-on-tval ... ...) ...)
     ...
|#

;; A world is a structure, (make-world t0 t1 t2 t3 t4 t5 t6 t7 t8),
;; where each field is a tval
(define-struct world (t0 t1 t2 t3 t4 t5 t6 t7 t8))

;; Sample worlds
(define WIN         (make-world 1 2 3
                                4 5 6
                                7 8 0))
(define A-WRLD      (make-world 1 5 2
                                4 0 3
                                7 8 6))

;; Function template f-on-world

#|
;; world ... --> ...
;; Purpose:
(define (f-on-world a-world ...)
  (... (f-on-tval (world-t0))
       (f-on-tval (world-t1))
       (f-on-tval (world-t2))
       (f-on-tval (world-t3))
       (f-on-tval (world-t4))
       (f-on-tval (world-t5))
       (f-on-tval (world-t6))
       (f-on-tval (world-t7))
       (f-on-tval (world-t8)) ...))

;; Sample expressions for f-on-world
(define WIN-VAL ...)
(define A-WORLD-VAL ...)

;; Tests using sample computations for f-onworld
(check-expect (f-on-world WIN ...)     WIN-VAL)
(check-expect (f-on-world A-WORLD ...) A-WORLD-VAL)

;; Tests using sample values for f-onworld
(check-expect (f-on-world ... ...) ...)
     ...
|#

;; A valid key, vk, is either
;; 1. "up"
;; 2. "down"
;; 3. "left"
;; 4. "right"
;; 5. " "

;; Sample vks
(define UP    "up")
(define DOWN  "down")
(define LEFT  "left")
(define RIGHT "right")
(define HKEY  " ")

;; Function template for f-on-vk
#|
;; vk ... --> ...
;; Purpose:
(define (f-on-vk a-vk ...)
  (cond [(key=? a-vk UP)    ...]
        [(key=? a-vk DOWN)  ...]
        [(key=? a-vk LEFT)  ...]
        [(key=? a-vk RIGHT) ...]
        [else ...]))

;; Tests for f-on-vk
(check-expect (f-on-vk UP    ...) ...)
(check-expect (f-on-vk DOWN  ...) ...)
(check-expect (f-on-vk LEFT  ...) ...)
(check-expect (f-on-vk RIGHT ...) ...)
(check-expect (f-on-vk HKEY  ...) ...)
|#

; draw-world: world --> scene
; Purpose: To draw the given world in the empty-scene
(define (draw-world a-world)
  (local [;; tval --> image
          ;; Purpose: Return the given BV's tile image
          (define (tile-img a-tval)
            (cond [(= a-tval 0) T0]
                  [(= a-tval 1) T1]
                  [(= a-tval 2) T2]
                  [(= a-tval 3) T3]
                  [(= a-tval 4) T4]
                  [(= a-tval 5) T5]
                  [(= a-tval 6) T6]
                  [(= a-tval 7) T7]
                  [else T8]))]
    (above (beside (tile-img (world-t0 a-world))
                   (tile-img (world-t1 a-world))
                   (tile-img (world-t2 a-world)))
           (beside (tile-img (world-t3 a-world))
                   (tile-img (world-t4 a-world))
                   (tile-img (world-t5 a-world)))
           (beside (tile-img (world-t6 a-world))
                   (tile-img (world-t7 a-world))
                   (tile-img (world-t8 a-world))))))


;; Tests using sample values for draw-world
(check-expect (draw-world WIN) .)
(check-expect (draw-world A-WRLD) .)

(check-expect (draw-world (make-world 5 2 3 1 8 6 4 0 7)) .)


;; world key --> world
;; Purpose: Return next world after a key event
(define (process-key a-world a-key)
  (local [;; key --> Boolean
          ;; Purpose: Determine in given key is a vk
          (define (vk? a-key)
            (or (key=? a-key UP)
                (key=? a-key DOWN)
                (key=? a-key LEFT)
                (key=? a-key RIGHT)
                (key=? a-key HKEY)))

          ;; world vk --> world
          ;; Purpose: Return the next world after given vk
          (define (process-vk a-world a-vk)
            (local [;; world --> bpos
                    ;; Purpose: Return the bpos for the blank
                    (define (blank-pos a-world) 
                      (cond [(= (world-t0 a-world) 0) 0]
                            [(= (world-t1 a-world) 0) 1]
                            [(= (world-t2 a-world) 0) 2]
                            [(= (world-t3 a-world) 0) 3]
                            [(= (world-t4 a-world) 0) 4]
                            [(= (world-t5 a-world) 0) 5]
                            [(= (world-t6 a-world) 0) 6]
                            [(= (world-t7 a-world) 0) 7]
                            [(= (world-t8 a-world) 0) 8]))
                    
                    ;; world vk --> bpos
                    ;; Purpose: Return the bpos to move the blank into
                    (define (get-target-bpos a-world a-vk)
                      (local [(define BLNK-POS (blank-pos a-world))]
                        (cond [(key=? a-vk UP)
                               (if (< BLNK-POS 3)
                                   BLNK-POS
                                   (- BLNK-POS 3))]
                              [(key=? a-vk DOWN)
                               (if (> BLNK-POS 5)
                                   BLNK-POS
                                   (+ BLNK-POS 3))]
                              [(key=? a-vk LEFT)
                               (if (= (remainder BLNK-POS 3) 0)
                                   BLNK-POS
                                   (sub1 BLNK-POS))]
                              [else
                               (if (= (remainder BLNK-POS 3) 2)
                                   BLNK-POS
                                   (add1 BLNK-POS))])))

                    ;; world bpos --> world
                    ;; Purpose: Move the blank to the given bpos
                    (define (swap-empty a-world target)
                      (local [;; world bpos --> bval
                              ;; Purpose: Return new value of given bpos
                              (define (new-tile-value a-world a-bpos)
                                (local [;; world bpos --> bval
                                        ;; Purpose: Return the given bpos' bval
                                        (define (get-tile-value a-world a-bpos)
                                          (cond [(= a-bpos 0) (world-t0 a-world)]
                                                [(= a-bpos 1) (world-t1 a-world)]
                                                [(= a-bpos 2) (world-t2 a-world)]
                                                [(= a-bpos 3) (world-t3 a-world)]
                                                [(= a-bpos 4) (world-t4 a-world)]
                                                [(= a-bpos 5) (world-t5 a-world)]
                                                [(= a-bpos 6) (world-t6 a-world)]
                                                [(= a-bpos 7) (world-t7 a-world)]
                                                [else (world-t8 a-world)]))]
                                  (cond [(= target a-bpos) 0]
                                        [(= (blank-pos a-world) a-bpos) (get-tile-value a-world target)]
                                        [else (get-tile-value a-world a-bpos)])))]
                        (make-world
                         (new-tile-value a-world 0)
                         (new-tile-value a-world 1)
                         (new-tile-value a-world 2)
                         (new-tile-value a-world 3)
                         (new-tile-value a-world 4)
                         (new-tile-value a-world 5)
                         (new-tile-value a-world 6)
                         (new-tile-value a-world 7)
                         (new-tile-value a-world 8))))
                    (define neighbors '((1 3)
                                        (4 0 2)
                                        (1 5)
                                        (0 4 6)
                                        (7 1 3 5)
                                        (2 4 8)
                                        (3 7)
                                        (8 4 6)
                                        (5 7)))

                    ;; world → natnum
                    ;; Purpose: Compute the Manhattan distance
                    (define (manhattan-distance a-world)
                      (local [;; bpos → natnum Purpose: Sum the tile distances
                              (define (sum-distances a-bpos)
                                (local [;; world bpos → bval Purpose: Return given bpos’ bval
                                        (define (tile-value a-bpos)
                                          (cond [(= a-bpos 0) (world-t0 a-world)]
                                                [(= a-bpos 1) (world-t1 a-world)]
                                                [(= a-bpos 2) (world-t2 a-world)]
                                                [(= a-bpos 3) (world-t3 a-world)]
                                                [(= a-bpos 4) (world-t4 a-world)]
                                                [(= a-bpos 5) (world-t5 a-world)]
                                                [(= a-bpos 6) (world-t6 a-world)]
                                                [(= a-bpos 7) (world-t7 a-world)]
                                                [else (world-t8 a-world)]))
                                        ;; tval → bpos Purpose: Return WIN bpos for given tval
                                        (define (final-bpos a-tval)
                                          (if (= a-tval 0) 8 (sub1 a-tval)))
                                        ;; bpos → natnum Purpose: Return bpos’ row
                                        (define (get-row a-bpos) (quotient a-bpos 3))
                                        ;; bpos → natnum Purpose: Return bpos’ column
                                        (define (get-col a-bpos) (remainder a-bpos 3))
                                        ;; bpos bpos → natnum Purpose: Return distance
                                        (define (distance bpos1 bpos2)
                                          (if (= (tile-value bpos1) 0) 0
                                              (+ (abs (- (get-row bpos1) (get-row bpos2)))
                                                 (abs (- (get-col bpos1) (get-col bpos2))))))
                                        (define win-bpos (final-bpos (tile-value a-bpos)))]
                                  (if (= a-bpos 0)
                                      (distance a-bpos win-bpos)
                                      (+ (distance a-bpos win-bpos) (sum-distances (sub1 a-bpos))))))]
                        (sum-distances 8)))

                    ;; (listof (listof world)) (listof world) → (listof world)
                    ;; Purpose: Return sequence of moves to WIN
                    ;; How: If the best path’s first world is WIN then return
                    ;; the reverse of the best path. Otherwise, continue
                    ;; the search with:
                    ;; 1. a new list of paths is obtained by removing the
                    ;; best path and adding paths constructed using
                    ;; the unvisited successors of the best path’s
                    ;; first world and the best path.
                    ;; 2. an accumulator obtained by adding the best
                    ;; path’s first world to the given accumulator.
                    ;; Accumulator Invariant:
                    ;; visited = worlds whose successors are part of the search

                    (define (find-solution-a* a-llow visited)
                      (local [(define best-path 
                                (foldl
                                 (λ (p accum)
                                   (if (< (manhattan-distance (first p))
                                          (manhattan-distance (first accum)))
                                       p
                                       accum))
                                 (first a-llow)
                                 (rest a-llow)))
                              (define first-world (first best-path))]
                        (if (equal? first-world WIN)
                            (reverse best-path)
                            (local 
                              [(define successors
                                 (filter
                                  (λ (w) (not (member? w visited)))
                                  (map (λ (neigh) (swap-empty first-world neigh))
                                       (list-ref neighbors
                                                 (blank-pos first-world)))))
                               (define new-paths (map (λ (w) (cons w best-path))
                                                      successors))
                               (define new-llow (append (remove best-path a-llow)
                                                        new-paths))]
                              (find-solution-a* new-llow (cons first-world visited))))))

                    ;; world → world
                    ;; Purpose: Make a move for the player
                    (define (make-move a-world)
                      (if (equal? a-world WIN)
                          a-world
                          (second (find-solution-a* (list (list a-world))
                                                       '()))))]         
              (if (or (key=? a-vk UP) (key=? a-vk DOWN) (key=? a-vk LEFT) (key=? a-vk RIGHT))
                  (swap-empty a-world (get-target-bpos a-world a-vk))
                  (make-move a-world))))]
    (if (vk? a-key)
        (process-vk a-world a-key)
        a-world)))

;; Tests for process-key
(check-expect (process-key A-WRLD "m") A-WRLD)
(check-expect (process-key A-WRLD "d") A-WRLD)
(check-expect (process-key WIN UP)
              (make-world 1 2 3
                          4 5 0
                          7 8 6))
(check-expect (process-key (make-world 0 1 2
                                       3 4 5
                                       6 7 8)
                           UP)
              (make-world 0 1 2
                          3 4 5
                          6 7 8))
(check-expect (process-key (make-world 0 1 2
                                       3 4 5
                                       6 7 8)
                           DOWN)
              (make-world 3 1 2
                          0 4 5
                          6 7 8))
(check-expect (process-key WIN DOWN) WIN)
(check-expect (process-key A-WRLD LEFT)
              (make-world 1 5 2
                          0 4 3
                          7 8 6))
(check-expect (process-key (make-world 1 5 2
                                       0 4 3
                                       7 8 6)
                           LEFT)
              (make-world 1 5 2
                          0 4 3
                          7 8 6))
(check-expect (process-key A-WRLD RIGHT)
              (make-world 1 5 2
                          4 3 0
                          7 8 6))
(check-expect (process-key WIN RIGHT) WIN)
(check-expect (process-key (make-world 1 2 3
                                       4 5 6
                                       7 0 8)
                           HKEY)
              (make-world 1 2 3
                          4 5 6
                          7 8 0))
(check-expect (process-key (make-world 2 3 0
                                         1 5 6
                                         4 7 8)
                             HKEY)
                (make-world 2 0 3
                            1 5 6
                            4 7 8))

;; world --> Boolean
;; Purpose: Determine if the game has ended
(define (game-over? a-world) (equal? a-world WIN))

;; Sample expressions for game-over?
(define WIN-OVER (equal? WIN WIN))
(define A-WRLD-OVER (equal? A-WRLD WIN))

;; Tests using sample computations for game-over? 
(check-expect (game-over? A-WRLD) A-WRLD-OVER)
(check-expect (game-over? WIN)    WIN-OVER)

;; Tests using sample values for game-over?
(check-expect (game-over? (make-world 5 2 3 1 8 6 4 0 7)) #false)
(check-expect (game-over? (make-world 4 3 1 0 7 2 8 5 6)) #false)

;; world --> image
;; Purpose: Draw the final world
(define (draw-last-world a-world)
  (overlay/xy (text "PUZZLE SOLVED!!!" 25 'gold)
              -40
              -70
              (draw-world a-world)))

;; Sample expressions for draw-last-world
(define WIN-FIMG (overlay/xy (text "PUZZLE SOLVED!!!" 25 'gold)
                             -40
                             -70
                             (draw-world WIN)))

(define A-WRLD-FIMG (overlay/xy (text "PUZZLE SOLVED!!!" 25 'gold)
                                -40
                                -70
                                (draw-world A-WRLD)))

;; Tests using sample computations for draw-last-world
(check-expect (draw-last-world WIN)    WIN-FIMG)
(check-expect (draw-last-world A-WRLD) A-WRLD-FIMG)

;; Tests using sample values for draw-last-world
(check-expect (draw-last-world (make-world 4 3 1 0 7 2 8 5 6)) .)
(check-expect (draw-last-world (make-world 5 2 3 1 8 6 4 0 7)) .)

;; A name is either a symbol or a string

;; name --> world
;; Purpose: Run the 8-puzzle game
(define (run a-name)
  (big-bang
      #;A-WRLD
      (make-world 1 8 0
                  4 3 2
                  5 7 6) ;; takes too long for the first couple of moves :(
    (on-draw draw-world)
    (on-key process-key)
    (stop-when game-over? draw-last-world)
    (name a-name)))
