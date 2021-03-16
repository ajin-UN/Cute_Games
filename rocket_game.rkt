;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname BreakOut) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;Level 1:
; we will have  6 bricks in the background and the initial speed of the ball will be (0,2)
;Level 2:
; we will have 12 bricks in the background and the initial speed of the ball will be (0,2)
;Level 3:
; we will have 18 bricks in the background and the initial speed of the ball will be (0,4)
;Level 4:
; we will have 24 bricks in the background and the initial speed of the ball will be (0,8)

; A Break-world is
;(make-fw paddle
;         width
;         Bricks-property
;         Ball-property
;         Natural)
; interp.: if `a-bw` is a Break-world then all of:
; - (bw-paddle a-bw) is the x coordinate of the paddle,
; - (bw-width a-bw) is the width paddle,
; - (bw-bricks a-bw) contains a list of information about the Bricks,
; - (bw-balls a-bw) contains a list of information about the Balls,
; - (bw-level a-bw) contains a list of level right now,
; - (bw-score a-bw) is the score.
(define-struct bw (paddle
                   width
                   bricks
                   balls
                   level
                   score))

; A Posn is (make-posn Real Real)
; (Note: `Real` means a real number, which excludes complex numbers.)

; a Bricks-property is
; (make-rp Posn Type)
; interpretation: if "a-bp" is a Bricks-property then all of:
; - (rp-posn a-fp) is the position of the brick,
; - (rp-type a-fp) is a number of which type the brick is
(define-struct brick (posn
                      type))
; a type is:
; normal: no special function
; add: add one ball in the game

; a Ball-property is
; (make-bp Posn Posn)
; interpretation: if "a-bp" is a Ball-property then all of:
; - (bp-posn a-pp) is the postion of the ball,
; - (bp-direction a-pp) gives which direction the ball is moving,
(define-struct ball (posn
                      direction))


;a Level is (make-level Ball Integer)
; (level-ball a-ball) is the new speed of the ball
; (level-brick a-brick) will initial the brick
(define-struct level (ball
                      brick))


(define WORLD-WIDTH 400)   ; window width
(define WORLD-HEIGHT 300)  ; window height
(define BACKGROUD-IMAGE (rectangle WORLD-WIDTH WORLD-HEIGHT "solid" "white"))
(define BALL-RIDUS 8)
(define BALL-IMAGE (circle BALL-RIDUS "solid" "black")) ; ball
(define PADDLE-WIDE 80)
(define PADDLE-TALL 12)
(define PADDLE-IMAGE (rectangle PADDLE-WIDE PADDLE-TALL "solid" "black"))  ; paddle
(define LEFT-SIDE 40)
(define RIGHT-SIDE 360)
(define BRICK-WIDE 50)
(define BRICK-TALL 15)
(define BRICK-IMAGE (rectangle BRICK-WIDE BRICK-TALL "outline" "black"))  ; brick

(define Level
  (list (make-level
         (make-ball (make-posn
                     (/ WORLD-WIDTH 2)
                     (- WORLD-HEIGHT 25))
                    (make-posn 0 -2))
         1)
        (make-level
         (make-ball (make-posn
                     (/ WORLD-WIDTH 2)
                     (- WORLD-HEIGHT 25))
                    (make-posn 0 -4))
         2)
        (make-level
         (make-ball (make-posn
                     (/ WORLD-WIDTH 2)
                     (- WORLD-HEIGHT 25))
                    (make-posn 0 -8))
         3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; draw: Break-world -> Image
; to show the images on screen
; Examples:
(check-expect (draw
               (make-bw
                (/ WORLD-WIDTH 2)
                PADDLE-WIDE
                '()
                '()
                (rest Level)
                0))
              (place-image
               (text "You Lose" 40 "black")
               200 150
               (place-image
                (text (number->string 0) 20 "black")
                25 25
                (place-image
                 PADDLE-IMAGE
                 (/ WORLD-WIDTH 2) (- WORLD-HEIGHT (/ PADDLE-TALL 2))
                 BACKGROUD-IMAGE))))
(define (draw tw)
  (cond
    [(empty? (bw-balls tw))
     (place-image
      (text "You Lose" 40 "black")
      200 150
     (place-image
      (text (number->string (bw-score tw)) 20 "black")
      25 25
      (place-image
       PADDLE-IMAGE
       (bw-paddle tw) (- WORLD-HEIGHT (/ PADDLE-TALL 2))
       (draw-ball (bw-balls tw) tw))))]
    [else
     (place-image
      (text (number->string (bw-score tw)) 20 "black")
      25 25
      (place-image
       PADDLE-IMAGE
       (bw-paddle tw) (- WORLD-HEIGHT (/ PADDLE-TALL 2))
       (draw-ball (bw-balls tw) tw)))]))

; draw-ball: [LIST-OF-BALLS] -> Image
; to show the balls on screen
; Examples:
(check-expect (draw-ball
               (list (make-ball
                      (make-posn 50 50)
                      (make-posn 0 2)))
               (make-bw
                (/ WORLD-WIDTH 2)
                PADDLE-WIDE
                '()
                (list (make-ball (make-posn
                                  50
                                  50)
                                 (make-posn 0 2)))
                (rest Level)
                0))
              (place-image
               BALL-IMAGE
               50
               50
               BACKGROUD-IMAGE))
; Strategy: structure decomposition
(define (draw-ball balls tw)
  (cond
    [(empty? balls) (draw-bricks (bw-bricks tw))]
    [else
     (place-image
      BALL-IMAGE
      (posn-x (ball-posn (first balls)))
      (posn-y (ball-posn (first balls)))
      (draw-ball (rest balls) tw))]))
 
; draw-brick: [LIST-OF-BRICKS] -> Image
; to show the bricks on screen
; Examples:
(check-expect (draw-bricks (list (make-brick (make-posn 100 75) "normal")))
              (place-image
               BRICK-IMAGE
               100
               75
               BACKGROUD-IMAGE))
               
; Strategy: structure decomposition
(define (draw-bricks bricks)
  (cond
    [(empty? bricks) BACKGROUD-IMAGE]
    [else
     (place-image
      BRICK-IMAGE
      (posn-x (brick-posn (first bricks)))
      (posn-y (brick-posn (first bricks)))
      (draw-bricks (rest bricks)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mouse: Break-world MouseEvent -> Break-world
; change direction when you move your mouse
; Examples:
(check-expect (mouse
               (make-bw
                (/ WORLD-WIDTH 2) PADDLE-WIDE
                '() '() Level 0)
               50 50 "move")
              (make-bw
               50 PADDLE-WIDE '() '() Level 0))
(check-expect (mouse
               (make-bw
                (/ WORLD-WIDTH 2) PADDLE-WIDE
                '() '() Level 0)
               10000 50 "move")
              (make-bw
               (/ WORLD-WIDTH 2) PADDLE-WIDE '() '() Level 0))
    
; Strategy: structure decomposition
(define (mouse tw x-mouse y-mouse cur-event)
  (cond
    [(and (string=? cur-event "move")
          (<= LEFT-SIDE x-mouse RIGHT-SIDE))
     (make-bw
      x-mouse
      (bw-width tw)
      (bw-bricks tw)
      (bw-balls tw)
      (bw-level tw)
      (bw-score tw))]
    [else tw]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tick: Break-world -> Break-world
; update the status after every tick
(define (tick tw)
  (level-up?
   (make-bw
    (bw-paddle tw)
    (bw-width tw)
    (bricks-remove (bw-bricks tw) (bw-balls tw))
    (add-balls (balls-move (bw-balls tw) tw) tw)
    (bw-level tw)
    (count-score tw))))


; level-up?: Break-world -> Break-world
; judge whether there is no brick left and we should level up
; Examples:
(check-expect (level-up?
               (make-bw
                 (/ WORLD-WIDTH 2)
                 PADDLE-WIDE
                 '()
                 (list (make-ball (make-posn
                                   (/ WORLD-WIDTH 2)
                                   (- WORLD-HEIGHT 25))
                                  (make-posn 0 -2)))
                 Level
                 0)) 
              (make-bw
                (/ WORLD-WIDTH 2)
                PADDLE-WIDE
                (initial-bricks 1 6 50 75 '())
                (list (make-ball (make-posn
                                  (/ WORLD-WIDTH 2)
                                  (- WORLD-HEIGHT 25))
                                 (make-posn 0 -2)))
                (rest Level)
                50))
(define (level-up? tw)
  (if (empty? (bw-bricks tw))
      (make-bw
       (/ WORLD-WIDTH 2)
       PADDLE-WIDE
       (initial-bricks (level-brick (first (bw-level tw))) 6 50 75 '())
       (list (level-ball (first (bw-level tw))))
       (rest (bw-level tw))
       (+ (bw-score tw) 50))
      tw))

;count-score: BreakWorld-> Integer
; count the score
(define (count-score tw)
  (less-zero? (- (+ (bw-score tw)
                    (* 10 (count-hitted-bricks
                           (bw-bricks tw)
                           (bw-balls tw)
                           0)))
                 (* 50 (count-losed-balls
                        (bw-balls tw) 0)))))

; less-zero?: Integer -> Integer
; judge score is less than zero
;examples:
(check-expect (less-zero? 1) 1)
(check-expect (less-zero? -1) 0)
(define (less-zero? score)
  (if (> score 0) score 0))
  
;count-losed-balls: [List-of-Balls] Integer -> Integer
; count how many balls we lose
(define (count-losed-balls balls num)
  (cond
    [(empty? balls) num]
    [else
     (if (>= (+ (posn-y (ball-posn (first balls)))
                (posn-y (ball-direction (first balls))))
             300)
         (count-losed-balls (rest balls) (+ 1 num))
         (count-losed-balls (rest balls) num))]))

;count-hitted-bricks: [List-of-Bricks] [List-of-Balls] Integer -> Integer
;count how many balls hit the brick
(define (count-hitted-bricks bricks balls num)
  (cond
    [(empty? balls) num]
    [else
     (if (ball-hit-brick? (ball-posn (first balls))
                      (ball-direction (first balls))
                      bricks)
         (count-hitted-bricks bricks (rest balls) (+ 1 num))
         (count-hitted-bricks bricks (rest balls) num))]))
     

; bricks-remove: [List-of-Bricks] [List-of-Balls] -> [List-of-Bricks]
; remove bricks which is hitted by ball
; Examples:
(check-expect (bricks-remove
               (list (make-brick (make-posn 50 75) "normal")
                     (make-brick (make-posn 100 75) "normal"))
               (list(make-ball (make-posn 102 75)
                          (make-posn 0 2))))
              (list (make-brick (make-posn 50 75) "normal")))
; Strategy: structure decomposition
(define (bricks-remove bricks balls)
  (cond
    [ (empty? balls) bricks ]
    [else 
     (bricks-remove
      (remove-bricks? (ball-posn (first balls)) (ball-direction (first balls)) bricks)
      (rest balls))]))


; remove-bricks? Posn Posn [List-of-Bricks] -> Boolean
; judges whether balls have hit walls
; Examples:
(check-expect (remove-bricks?
               (make-posn 102 75)
               (make-posn 0 2)
               (list (make-brick (make-posn 50 75) "normal")
                     (make-brick (make-posn 100 75) "normal")))
              (list (make-brick (make-posn 50 75) "normal")))
; Strategy: structure decomposition
(define (remove-bricks? ball direction bricks)
  (cond
    [(empty? bricks) '()]
    [else
     (if
      (and (<= (- (posn-x (brick-posn (first bricks))) (/ BRICK-WIDE 2)) (+ (posn-x ball) BALL-RIDUS (posn-x direction)))
           (>= (+ (posn-x (brick-posn (first bricks))) (/ BRICK-WIDE 2)) (- (+ (posn-x ball) (posn-x direction)) BALL-RIDUS))
           (<= (- (posn-y (brick-posn (first bricks))) (/ BRICK-TALL 2)) (+ (posn-y ball) BALL-RIDUS (posn-y direction)))
           (>= (+ (posn-y (brick-posn (first bricks))) (/ BRICK-TALL 2)) (- (+ (posn-y ball) (posn-y direction)) BALL-RIDUS)))
      (rest bricks)
      (cons (first bricks) (remove-bricks? ball direction (rest bricks))))]))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-balls balls tw)
  (cond
    [ (empty? balls) '() ]
    [else
     (if
      (ball-hit-brick? (ball-posn (first balls)) (ball-direction (first balls)) (bw-bricks tw))
      (if (equal? "add" (brick-type
                         (find-hitted-brick
                          (ball-posn (first balls))
                          (ball-direction (first balls))
                          (bw-bricks tw))))
          (cons (make-ball (make-posn
                            (/ WORLD-WIDTH 2)
                            (- WORLD-HEIGHT 25))
                           (make-posn 0 -2))
                (cons (first balls) (add-balls (rest balls) tw)))
          (cons (first balls) (add-balls (rest balls) tw)))
      (cons (first balls) (add-balls (rest balls) tw)))]))
          
  
  
; balls-move [List-of-Ball] Break-world -> [List-of-Ball]
; Move all the balls to the toward the direction they have
; Examples:
(check-expect (balls-move '()
                          (make-bw
                           (/ WORLD-WIDTH 2)
                           PADDLE-WIDE
                           (initial-bricks 0 6 50 75 '())
                           '()
                           Level
                           0))
              '())
(check-expect (balls-move (list (make-ball (make-posn
                                             (/ WORLD-WIDTH 2)
                                             (- WORLD-HEIGHT 25))
                                            (make-posn 0 2)))
                          (make-bw
                           (/ WORLD-WIDTH 2)
                           PADDLE-WIDE
                           (initial-bricks 0 6 50 75 '())
                           (list (make-ball (make-posn
                                             (/ WORLD-WIDTH 2)
                                             (- WORLD-HEIGHT 25))
                                            (make-posn 0 2)))
                           Level
                           0))
              (list (make-ball (make-posn
                                (/ WORLD-WIDTH 2)
                                (+ 2 (- WORLD-HEIGHT 25)))
                               (make-posn 0 2))))
(check-expect (balls-move (list (make-ball (make-posn
                                             (/ WORLD-WIDTH 2)
                                             300)
                                            (make-posn 0 2)))
                          (make-bw
                           (/ WORLD-WIDTH 2)
                           PADDLE-WIDE
                           (initial-bricks 0 6 50 75 '())
                           (list (make-ball (make-posn
                                             (/ WORLD-WIDTH 2)
                                             300)
                                            (make-posn 0 2)))
                           Level
                           0))
              '())
; Strategy: structure decomposition
(define (balls-move balls tw)
  (cond
    [ (empty? balls) '() ]
    [else
     (if (> (+ (posn-y (ball-posn (first balls)))
               (posn-y (ball-direction (first balls))))
            300)
         (balls-move (rest balls) tw)
         (cons (make-ball (make-posn (+ (posn-x (ball-posn (first balls)))
                                        (posn-x (ball-direction (first balls))))
                                     (+ (posn-y (ball-posn (first balls)))
                                        (posn-y (ball-direction (first balls)))))
                          (change-direction (first balls) tw))
               (balls-move (rest balls) tw)))]))
 
; change-direction Ball Break-world -> Direction
; change-direction when ball hits something
; Examples:
(check-expect (change-direction
               (make-ball (make-posn
                           (/ WORLD-WIDTH 2)
                           (- WORLD-HEIGHT 25))
                          (make-posn 0 2))
               (make-bw
                (/ WORLD-WIDTH 2)
                PADDLE-WIDE
                (initial-bricks 0 6 50 75 '())
                (list (make-ball (make-posn
                                  (/ WORLD-WIDTH 2)
                                  (- WORLD-HEIGHT 25))
                                 (make-posn 0 2)))
                Level
                0))
              (make-posn 0 2))
(check-expect (change-direction
               (make-ball (make-posn
                           (/ WORLD-WIDTH 2)
                           0)
                          (make-posn 0 2))
               (make-bw
                (/ WORLD-WIDTH 2)
                PADDLE-WIDE
                (initial-bricks 0 6 50 75 '())
                (list (make-ball (make-posn
                                  (/ WORLD-WIDTH 2)
                                  (- WORLD-HEIGHT 25))
                                 (make-posn 0 2)))
                Level
                0))
              (make-posn 0 -2))
(check-expect (change-direction
               (make-ball (make-posn
                           70
                           70)
                          (make-posn 1 2))
               (make-bw
                (/ WORLD-WIDTH 2)
                PADDLE-WIDE
                (list (make-brick (make-posn 100 75) "normal"))
                (list (make-ball (make-posn 70 70)
                                 (make-posn 1 2)))
                Level
                0))
              (make-posn -1 2))
(check-expect (change-direction
               (make-ball (make-posn
                           200
                           290)
                          (make-posn 1 -2))
               (make-bw
                (/ WORLD-WIDTH 2)
                PADDLE-WIDE
                (list (make-brick (make-posn 100 75) "normal"))
                (list (make-ball (make-posn 200 290)
                                 (make-posn 1 -2)))
                Level
                0))
              (make-posn 1 2))
(check-expect (change-direction
               (make-ball (make-posn
                           0
                           80)
                          (make-posn 1 2))
               (make-bw
                (/ WORLD-WIDTH 2)
                PADDLE-WIDE
                (list (make-brick (make-posn 100 75) "normal"))
                (list (make-ball (make-posn 0 80)
                                 (make-posn 1 2)))
                Level
                0))
              (make-posn -1 2))
; Strategy: decesion tree
(define (change-direction ball tw)
  (cond
    [ (ball-hit-paddle? (ball-posn ball) (ball-direction ball) (bw-paddle tw) (bw-width tw))
      (make-posn (+ (posn-x (ball-direction ball)) (/ (- (posn-x (ball-posn ball)) (bw-paddle tw)) 40)) 
                 (-(posn-y (ball-direction ball))))]
    [(ball-hit-wall? (ball-posn ball) (ball-direction ball))
     (if (< (+ (posn-y (ball-posn ball)) (posn-y (ball-direction ball))) BALL-RIDUS)
         (make-posn (posn-x (ball-direction ball)) (-(posn-y (ball-direction ball))))
         (make-posn (-(posn-x (ball-direction ball))) (posn-y (ball-direction ball))))]
    [(ball-hit-brick? (ball-posn ball) (ball-direction ball) (bw-bricks tw))
     (helper-change-direction
      (find-hitted-brick (ball-posn ball) (ball-direction ball) (bw-bricks tw))
      (ball-posn ball)
      (ball-direction ball))]
    [else (ball-direction ball)]))
         
; ball-hit-paddle? Posn Number Natural -> Boolean
; judges whether balls have hit the paddle
; Examples:
(check-expect (ball-hit-paddle?
               (make-posn 102 75)
               (make-posn 0 2)
               50
               PADDLE-WIDE)
              #false)
(check-expect (ball-hit-paddle?
               (make-posn 102 295)
               (make-posn 0 -2)
               103
               PADDLE-WIDE)
              #true)
(define (ball-hit-paddle? ball direction paddle width)
  (and (<= (- paddle (/ PADDLE-WIDE 2)) (+ (posn-x ball) BALL-RIDUS (posn-x direction)))
       (>= (+ paddle (/ PADDLE-WIDE 2)) (- (+ (posn-x ball) (posn-x direction)) BALL-RIDUS))
       (<= (- WORLD-HEIGHT BALL-RIDUS PADDLE-TALL) (+ (posn-y ball) (posn-y direction)))))

; ball-hit-wall? Posn -> Boolean
; judges whether balls have hit walls
; Examples:
(check-expect (ball-hit-wall?
               (make-posn 102 75)
               (make-posn 0 2))
              #false)
(check-expect (ball-hit-wall?
               (make-posn 102 0)
               (make-posn 0 2))
              #true)
(define (ball-hit-wall? ball direction)
  (or (< (+ (posn-x ball) (posn-x direction)) BALL-RIDUS)
      (> (+ (posn-x ball) (posn-x direction)) (- WORLD-WIDTH BALL-RIDUS))
      (< (+ (posn-y ball) (posn-y direction)) BALL-RIDUS)))

; ball-hit-bricks? Posn Posn [List-of-Bricks] -> Boolean
; judges whether balls have hit walls
; Examples:
(check-expect (ball-hit-brick?
               (make-posn 102 75)
               (make-posn 0 2)
               '())
              #false)
(check-expect (ball-hit-brick?
               (make-posn 102 75)
               (make-posn 0 2)
               (list (make-brick (make-posn 50 75) "normal")
                     (make-brick (make-posn 100 75) "normal")))
              #true)
(check-expect (ball-hit-brick?
               (make-posn 102 200)
               (make-posn 0 2)
               (list (make-brick (make-posn 50 75) "normal")
                     (make-brick (make-posn 100 75) "normal")))
              #false)
; Strategy: structure decomposition
(define (ball-hit-brick? ball direction bricks)
  (cond
    [(empty? bricks) #false]
    [else
     (if
      (and (<= (- (posn-x (brick-posn (first bricks))) (/ BRICK-WIDE 2)) (+ (posn-x ball) BALL-RIDUS (posn-x direction)))
           (>= (+ (posn-x (brick-posn (first bricks))) (/ BRICK-WIDE 2)) (- (+ (posn-x ball) (posn-x direction)) BALL-RIDUS))
           (<= (- (posn-y (brick-posn (first bricks))) (/ BRICK-TALL 2)) (+ (posn-y ball) BALL-RIDUS (posn-y direction)))
           (>= (+ (posn-y (brick-posn (first bricks))) (/ BRICK-TALL 2)) (- (+ (posn-y ball) (posn-y direction)) BALL-RIDUS)))
      #true
      (ball-hit-brick? ball direction (rest bricks)))]))

; find-hitted-brick Posn Posn [List-of-Bricks] -> Brick
; judges whether balls have hit walls
; Examples:
(check-expect (find-hitted-brick
               (make-posn 102 75)
               (make-posn 0 2)
               (list (make-brick (make-posn 50 75) "normal")
                     (make-brick (make-posn 100 75) "normal")))
              (make-brick (make-posn 100 75) "normal"))
(define (find-hitted-brick ball direction bricks)
  (if
   (and (<= (- (posn-x (brick-posn (first bricks))) (/ BRICK-WIDE 2)) (+ (posn-x ball) BALL-RIDUS (posn-x direction)))
        (>= (+ (posn-x (brick-posn (first bricks))) (/ BRICK-WIDE 2)) (- (+ (posn-x ball) (posn-x direction)) BALL-RIDUS))
        (<= (- (posn-y (brick-posn (first bricks))) (/ BRICK-TALL 2)) (+ (posn-y ball) BALL-RIDUS (posn-y direction)))
        (>= (+ (posn-y (brick-posn (first bricks))) (/ BRICK-TALL 2)) (- (+ (posn-y ball) (posn-y direction)) BALL-RIDUS)))
   (first bricks)
   (find-hitted-brick ball direction (rest bricks))))

;helper-change-direction
;help the ball to change the direction
; Examples:
(check-expect (helper-change-direction
               (make-brick (make-posn 100 75) "normal")
               (make-posn 102 75)
               (make-posn 0 2))
              (make-posn 0 -2))
(check-expect (helper-change-direction
               (make-brick (make-posn 100 75) "normal")
               (make-posn 70 70)
               (make-posn 1 2))
              (make-posn -1 2))
(define (helper-change-direction brick ball direction)
  (if (and
       (<= (- (posn-x (brick-posn brick)) (/ BRICK-WIDE 2)) (+ (posn-x ball) (posn-x direction)))
       (>= (+ (posn-x (brick-posn brick)) (/ BRICK-WIDE 2)) (+ (posn-x ball) (posn-x direction))))
      (make-posn (posn-x direction) (-(posn-y direction)))
      (make-posn (-(posn-x direction)) (posn-y direction)))) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initial-bricks row column posn-x posn-y brick)
  (cond
    [(and (= row 0) (= column 0))
     (cons (make-brick (make-posn posn-x posn-y) "add") brick)]
    [(= column 0)
     (cons (make-brick (make-posn posn-x posn-y) "add")
           (initial-bricks (- row 1) 6 50 (+ posn-y 15) brick))]
    [else
     (cons (make-brick (make-posn posn-x posn-y) "normal")
           (initial-bricks row (- column 1) (+ 50 posn-x) posn-y brick))]))
    
(define (start _dummy)
  (big-bang (make-bw
             (/ WORLD-WIDTH 2)
             PADDLE-WIDE
             (initial-bricks 0 6 50 75 '())
             (list (make-ball (make-posn
                               (/ WORLD-WIDTH 2)
                               (- WORLD-HEIGHT 25))
                              (make-posn 0 -2)))
             Level
             0)
    [on-tick tick 1/100]
    [on-mouse mouse]
    [to-draw draw]))

(start 0)

















  