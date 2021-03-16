;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname final_project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;rules:
;     press space to jump and if the star drops out of the world game over...
;     you can press space more than once to make it jump higher
;     the will change if you press right or left will star is jumping
;     if the star is coral and trying to eat a blue treat, it dies... verce vice
;     you can only kill a monster by step on it not walking through it
;     you will get 10 points by eatting a treat
;     you will get 100 points by eatting a monster
;     you can only kill the final monster if you hava at least 1000 points
;     kill the final monster win

(require 2htdp/image)
(require 2htdp/universe)
;A Star-world is
; (make-sw Star Number [List-of Level])
; interp: if 'sw' is a Star-world then all of:
; - (sw-star sw) is a Star
; - (sw-score sw) is a score
; - (sw-levels sw) is a [list-of Level]

(define-struct sw (rstar score levels))

; A RStar is (make-rstar Posn Color Num)
; interp: if "Rstar" is a RStar then all of:
; - (rstar-posn rstar) is a posn
; - (rstar-color rstar) is a string to represent the color
; - (rstar-jump rstar) is a num to represnet its jumpong status

(define-struct rstar (posn color direction jump))

; A Monster is (make-monster Type Posn Life)
; interp: if "monster" is a Monster then all of:
; - (monster-type) is a Monster-type
; - (monster-posn) is a posn
; - (monster-life) is a Number

(define-struct monster (type posn life))

; A Treat is (make-treat color posn   )
; interp: if "treat" is a Treat then all of:
; - (treat-color) is a tring to represent the color
; - (treat-posn) is a posn

(define-struct treat (color posn))


; A Level is (make-level [list-of posn] [list-of Treats] [list-of Monsters])
; A Posn is (make-posn Real Real))
; A Monster-type is a string represents the image of Monster

(define-struct level (stairs treats monsters))

(define WORLD-WIDTH 800)   ; window width
(define WORLD-HEIGHT 150)  ; window height
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))
(define STAIR-WIDTH 20)
(define STAIR-HEIGHT 2)

(define STAIR-IMAGE (rectangle STAIR-WIDTH STAIR-HEIGHT "solid" "black"))

(define COLORS (list "OrangeRed" "Coral" "Gold" "Olive" "Lime" "Blue"))

(define BLUE (bitmap/file "blue.jpg"))
(define BROWN (bitmap/file "brown.jpg"))
(define GREEN (bitmap/file "green.jpg"))
(define CUTIE (bitmap/file "cutie.jpg"))
(define BOSS (bitmap/file "boss.jpg"))
(define BLUE-SMALL (scale/xy (/ 1 20) (/ 1 10) BLUE))
(define BROWN-SMALL (scale/xy (/ 1 20) (/ 1 10) BROWN))
(define GREEN-SMALL (scale/xy (/ 1 20) (/ 1 10) GREEN))
(define BOSS-SMALL (scale/xy (/ 1 20) (/ 1 10) BOSS))
(define CUTIE-SMALL (scale/xy (/ 1 20) (/ 1 10) CUTIE))

(define MONSTERS (list BLUE-SMALL BROWN-SMALL GREEN-SMALL CUTIE-SMALL))

;make-stairs: Number Posn Number -> [List-of Posn]
;return a list of points
(define (make-stairs dir start-point num)
  (cond
    [(equal? num 0) '()]
    [else (cons (make-posn (+ (posn-x start-point) 20) (+ (posn-y start-point) dir))
                (make-stairs dir
                             (make-posn (+ (posn-x start-point) 20) (+ (posn-y start-point) dir))
                             (- num 1)))]))
;test:
(check-expect (make-stairs 10 (make-posn 0 0) 3)
              (list (make-posn 20 10) (make-posn 40 20) (make-posn 60 30)))
(check-expect (make-stairs -10 (make-posn 40 40) 2)
              (list (make-posn 60 30) (make-posn 80 20)))



(define POSNS-A (make-stairs -10 (make-posn 0 150) 3))
(define POSNS-B (make-stairs 10 (make-posn 60 120) 1))
(define POSNS-C (make-stairs -10 (make-posn 80 130) 10))
(define POSNS-D (make-stairs 10 (make-posn 280 30) 8))
(define POSNS-E (make-stairs 0 (make-posn 440 110) 5))
(define POSNS-F (make-stairs -10 (make-posn 540 110) 7))
(define POSNS-G (make-stairs 10 (make-posn 680 40) 5))

(define POSNS-A2 (make-stairs 10 (make-posn 0 50) 7))
(define POSNS-B2 (make-stairs -10 (make-posn 140 120) 8))
(define POSNS-C2 (make-stairs 10 (make-posn 300 40) 6))
(define POSNS-D2 (make-stairs 0 (make-posn 420 100) 3))
(define POSNS-E2 (make-stairs -10 (make-posn 480 100) 7))
(define POSNS-F2 (make-stairs 10 (make-posn 620 30) 8))


(define POSNS-A3 (make-stairs -10 (make-posn 0 80) 3))
(define POSNS-B3 (make-stairs 10 (make-posn 60 50) 5))
(define POSNS-C3 (make-stairs -10 (make-posn 160 100) 7))
(define POSNS-D3 (make-stairs 10 (make-posn 300 30) 8))
(define POSNS-E3 (make-stairs 0 (make-posn 460 110) 4))
(define POSNS-F3 (make-stairs -10 (make-posn 540 110) 7))
(define POSNS-G3 (make-stairs 10 (make-posn 680 40) 5))

(define POSNS-A4 (make-stairs 0 (make-posn 10 140) 4))
(define POSNS-B4 (make-stairs -10 (make-posn 90 140) 8))
(define POSNS-C4 (make-stairs 10 (make-posn 250 60) 6))
(define POSNS-D4 (make-stairs -10 (make-posn 370 120) 5))
(define POSNS-E4 (make-stairs 10 (make-posn 470 70) 6))
(define POSNS-F4 (make-stairs -10 (make-posn 610 130) 5))
(define POSNS-G4 (make-stairs 0 (make-posn 710 80) 4))

;make-level-one-stairs: [List-of Posn][List-of Posn][List-of Posn][List-of Posn]
;                       [List-of Posn][List-of Posn][List-of Posn] -> [List-of Posn]
;returns a list of posn for level one
(define (make-level-one-stairs l1 l2 l3 l4 l5 l6 l7)
  (append l1 l2 l3 l4 l5 l6 l7))
;test
(check-expect (make-level-one-stairs (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2)
                                     (list 1 2))
              (list 1 2 1 2 1 2 1 2 1 2 1 2 1 2))

(define LEVEL-ONE-STAIRS (make-level-one-stairs POSNS-A POSNS-B POSNS-C POSNS-D POSNS-E POSNS-F POSNS-G))
(define LEVEL-TWO-STAIRS (append POSNS-A2 POSNS-B2 POSNS-C2 POSNS-D2 POSNS-E2 POSNS-F2))
(define LEVEL-THREE-STAIRS (append POSNS-A3 POSNS-B3 POSNS-C3 POSNS-D3 POSNS-E3 POSNS-F3 POSNS-G3))
(define LEVEL-FOUR-STAIRS (append POSNS-A4 POSNS-B4 POSNS-C4 POSNS-D4 POSNS-E4 POSNS-F4 POSNS-G4))


;make-treat-position: posn -> posn
;returns a position of treat that aboves the given posn
(define (make-treat-position a)
  (make-posn (posn-x a)
             (+(random (- (posn-y a) 15)) 10)))


;find-posn: [List-of Posn] Num -> Posn
;return a posn based on the given x
(define (find-posn l x)
  (cond
    [(equal? '() l) (make-posn 0 0)]
    [else (if (and (>= (+(posn-x (first l)) 10) x)
                   (< (-(posn-x (first l)) 10) x))
              (first l)
              (find-posn (rest l) x))]))
;test:
(check-expect (find-posn (list (make-posn 10 1)
                               (make-posn 30 2)
                               (make-posn 50 3)) 22)
              (make-posn 30 2))
(check-expect (find-posn (list (make-posn 10 1)
                               (make-posn 30 2)
                               (make-posn 50 3)) 20)
              (make-posn 10 1))
(check-expect (find-posn (list (make-posn 10 1)
                               (make-posn 30 2)
                               (make-posn 50 3)) 200)
              (make-posn 0 0))
;find-previous-posn: [List-of Posn] Num -> Posn
;return the previous posn in the list
(define (find-previous-posn l x)
  (local
    [(define CUR-POSN
       (find-posn l x))]
    (find-posn l (- x 11))))
;tests;
(check-expect (find-previous-posn (list (make-posn 10 1)
                                        (make-posn 30 2)
                                        (make-posn 50 3)) 22)
              (make-posn 10 1))
(check-expect (find-previous-posn (list (make-posn 10 1)
                                        (make-posn 30 2)
                                        (make-posn 50 3)) 45)
              (make-posn 30 2))
(check-expect (find-previous-posn (list (make-posn 10 1)
                                        (make-posn 30 2)
                                        (make-posn 50 3)) 200)
              (make-posn 0 0))


;make-treats-positions: [List-of Posn] -> [List-of Posn]
;returns a list of positions of treats based on the list of stairs
;strategy: structural decomposition
(define (make-treats-positions stairs)
    (cond
      [(empty? stairs) '()]
      [else
       (if (zero? (random 3))
           (cons (make-treat-position (first stairs))
                 (make-treats-positions (rest stairs)))
           (make-treats-positions (rest stairs)))]))
;make-monsters-positions: [List-of Posn] -> [List-of Posn]
;return a list of positions of monsters based on the list of stairs
(define (make-monsters-positions stairs)
  (cond
    [(empty? stairs) '()]
    [else
     (if (zero? (random 5))
         (cons (make-posn (posn-x (first stairs))
                          (- (posn-y (first stairs)) 8))
               (make-monsters-positions (rest stairs)))
         (make-monsters-positions (rest stairs)))]))

;make-monsters-position-four: [List-of Posn] -> [List-of Posn]
;return a list of positions of monsters but not in the last flat stairs
(define (make-monsters-position-four stairs)
  (local
    [(define STOP
       (list-ref stairs (- (length stairs) 4)))]
    (cond
      [(equal? (first stairs) STOP) '()]
      [else
       (if (zero? (random 5))
           (cons (make-posn (posn-x (first stairs))
                            (- (posn-y (first stairs)) 8))
                 (make-monsters-position-four (rest stairs)))
           (make-monsters-position-four (rest stairs)))])))



;make-monsters: [List-of Posn] -> [List-of Monster]
;return a list of monsters
(define (make-monsters l)
  (cond
    [(empty? l) '()]
    (else
     (cons (make-monster (list-ref MONSTERS (random 4)) (first l) 100)
           (make-monsters (rest l))))))
                         
;make-treats: [List-of Posn] -> [List-of Treat]
;return a list of treat
(define (make-treats l)
  (cond
    [(empty? l) '()]
    [else
     (cons (make-treat (list-ref COLORS (random 6)) (first l))
           (make-treats (rest l)))]))


;trim-list: [List-Of posn] -> [List-Of posn]
;return a list of posns
(define (trim-list ps)
  (rest (rest (rest (rest ps)))))
;test:
(check-expect (trim-list (list (make-posn 1 1) (make-posn 2 2) (make-posn 3 3)
                               (make-posn 4 4) (make-posn 5 5) (make-posn 6 6)))
              (list (make-posn 5 5) (make-posn 6 6)))




;make-treat-image: Treat -> Imgae
;return a image
(define (make-treat-image tt)
  (circle 5 "solid" (treat-color tt)))
;tests:
(check-expect (make-treat-image (make-treat "red" (make-posn 5 5)))
              (circle 5 "solid" "red"))
         
(define LEVEL-ONE-TREATS (make-treats (make-treats-positions (trim-list LEVEL-ONE-STAIRS))))
(define LEVEL-TWO-TREATS (make-treats (make-treats-positions (trim-list LEVEL-TWO-STAIRS))))
(define LEVEL-THREE-TREATS (make-treats (make-treats-positions (trim-list LEVEL-TWO-STAIRS))))
(define LEVEL-FOUR-TREATS (make-treats (make-treats-positions (trim-list LEVEL-TWO-STAIRS))))

(define LEVEL-TWO-MONSTER (make-monsters (make-monsters-positions (trim-list LEVEL-TWO-STAIRS))))
(define LEVEL-THREE-MONSTER (make-monsters (make-monsters-positions (trim-list LEVEL-THREE-STAIRS))))
(define LEVEL-FOUR-M (make-monsters (make-monsters-position-four (trim-list LEVEL-FOUR-STAIRS))))

;make-boss-monster: [List-of Monster] -> [List-of Monster]
;return the level four monsters
(define (make-boss-monster lm)
  (cons (make-monster BOSS-SMALL
                      (make-posn 710 67)
                      1000)
        lm))

(define LEVEL-FOUR-MONSTER (make-boss-monster LEVEL-FOUR-M))

(define LEVEL-ONE (make-level LEVEL-ONE-STAIRS LEVEL-ONE-TREATS '()))
(define LEVEL-TWO (make-level LEVEL-TWO-STAIRS LEVEL-TWO-TREATS LEVEL-TWO-MONSTER))
(define LEVEL-THREE (make-level LEVEL-THREE-STAIRS LEVEL-THREE-TREATS LEVEL-THREE-MONSTER))
(define LEVEL-FOUR (make-level LEVEL-FOUR-STAIRS LEVEL-FOUR-TREATS LEVEL-FOUR-MONSTER))

(define LEVELS (list LEVEL-ONE LEVEL-TWO LEVEL-THREE LEVEL-FOUR))


(define STARTING-SW (make-sw (make-rstar (make-posn 20 135)
                                          "black" "right" 0)
                              0
                              LEVELS))
(define TEST-SW (make-sw (make-rstar (make-posn 66 115)
                                     "black" "right" 0)
                         0
                         LEVELS))

(define TEST-SW-F (make-sw (make-rstar (make-posn 20 80)
                                       "black" "right" 0)
                           0
                           (list LEVEL-FOUR)))


;find-which-stair: sw -> posn
;return a stair location of where is the star suppose to be
(define (find-which-stair sw)
  (cond
    [(equal? (level-stairs (first (sw-levels sw))) '())
     (make-posn 0 0)]
    [else
     (if (and (>= (posn-x (rstar-posn (sw-rstar sw)))
                  (- (posn-x  (first (level-stairs (first (sw-levels sw))))) 10))
              (< (posn-x (rstar-posn (sw-rstar sw)))
                  (+ (posn-x (first (level-stairs (first (sw-levels sw))))) 10)))              
         (first(level-stairs (first (sw-levels sw))))
         (find-which-stair (make-sw (sw-rstar sw)
                                    (sw-score sw)
                                    (cons (make-level (rest (level-stairs (first (sw-levels sw))))
                                                      (level-treats (first (sw-levels sw)))
                                                      (level-monsters (first (sw-levels sw))))
                                          (rest (sw-levels sw))))))]))

;test:
(check-expect (find-which-stair STARTING-SW) (make-posn 20 140))
(check-expect (find-which-stair (make-sw (make-rstar (make-posn 75 80) "red" "left" 0)
                                          0
                                          (sw-levels STARTING-SW)))
              (make-posn 80 130))

(check-expect (find-which-stair (make-sw (make-rstar (make-posn 70 80) "red" "left" 0)
                                          0
                                          (sw-levels STARTING-SW)))
              (make-posn 80 130))

;find-next-stair: sw -> posn
;return the next stair the star will be
(define (find-next-stair sw)
  (local
    [(define new-posn
       (find-which-stair sw))]
    (find-posn (level-stairs (first (sw-levels sw))) (+ 11 (posn-x new-posn)))))
;test
(check-expect (find-next-stair STARTING-SW) (make-posn 40 130))
(check-expect (find-next-stair TEST-SW) (make-posn 80 130))

;find-previous-stair: sw -> posn
;return the previous stair the star was
(define (find-previous-stair sw)
   (local
    [(define cur-posn
       (find-which-stair sw))]
    (find-previous-posn (level-stairs (first (sw-levels sw))) (posn-x cur-posn))))
;test
(check-expect (find-previous-stair STARTING-SW) (make-posn 0 0))
(check-expect (find-previous-stair TEST-SW) (make-posn 40 130))

;make-star-image: rstar -> image
;returns a star image
(define (make-star-image r)
  (star 10 "solid" (rstar-color r)))
;tests:
(check-expect (make-star-image (make-rstar (make-posn 4 2) "gray" "left" 0))
              (star 10 "solid" "gray"))

(check-expect (make-star-image (make-rstar (make-posn 4 2) "yellow" "left" 0))
              (star 10 "solid" "yellow"))

(check-expect (make-star-image (make-rstar (make-posn 4 2) "red" "left" 0))
              (star 10 "solid" "red"))


;touch?: Posn Posn -> Boolean
;returns a boolean to see if two elements touch
;startegy: domain knowledge
(define (star-touch-treat? s t)
  (and (< (abs (- (posn-x s) (posn-x t))) 3)
       (< (abs (- (posn-y s) (posn-y t))) 3)))
;tests:
(check-expect (star-touch-treat? (make-posn 2 2) (make-posn 2 3))
              #t)
(check-expect (star-touch-treat? (make-posn 3 3) (make-posn 1 1))
              #t)
(check-expect (star-touch-treat? (make-posn 3 3) (make-posn 10 10))
              #f)

;star-touch-monster?: Posn Posn -> Boolean
;returns if the star is stepping on top of the monster
(define (star-touch-monster? s m)
  (and (< (abs (- (posn-x s) (posn-x m))) 3)
       (and (< (posn-y s) (posn-y m)) (< (abs (- (posn-y s) (posn-y m))) 13))))
;test;
(check-expect (star-touch-monster? (make-posn 2 2) (make-posn 2 3)) #t)
(check-expect (star-touch-monster? (make-posn 2 2) (make-posn 2 1)) #f)


;update-treat-list: sw -> [List-of Treat]
;returns a new list of treats
(define (update-treat-list sw)
    (cond
      [(empty? (level-treats (first (sw-levels sw)))) '()]
      [else (if (star-touch-treat? (rstar-posn (sw-rstar sw))
                                   (treat-posn (first (level-treats (first (sw-levels sw))))))
                (update-treat-list (make-sw (sw-rstar sw)
                                            (sw-score sw)
                                            (cons (make-level (level-stairs (first (sw-levels sw)))
                                                              (rest (level-treats (first (sw-levels sw))))
                                                              (level-monsters (first (sw-levels sw))))
                                                  (rest (sw-levels sw)))))
                (cons (first (level-treats (first (sw-levels sw))))
                      (update-treat-list (make-sw (sw-rstar sw)
                                                  (sw-score sw)
                                                  (cons (make-level (level-stairs (first (sw-levels sw)))
                                                                    (rest (level-treats (first (sw-levels sw))))
                                                                    (level-monsters (first (sw-levels sw))))
                                                        (rest (sw-levels sw)))))))]))
;tests:
(check-expect (update-treat-list (make-sw (make-rstar (make-posn 2 2) "Oliver" "left" 0)
                                          0
                                          (list (make-level '()
                                                            (list (make-treat "gold" (make-posn 2 1))
                                                                  (make-treat "gold" (make-posn 2 3)))
                                                            '())
                                                '())))
             '())
                                                           

(check-expect (update-treat-list (make-sw (make-rstar (make-posn 2 2) "Oliver" "left" 0)
                                          0
                                          (list (make-level '()
                                                            (list (make-treat "gold" (make-posn 2 1))
                                                                  (make-treat "gold" (make-posn 2 3))
                                                                  (make-treat "gold" (make-posn 10 10)))
                                                            '())
                                                '())))
               (list (make-treat "gold" (make-posn 10 10))))

;update-monster: sw -> [List-of Monster]
;return a list of monsters
(define (update-monster sw l)
  (cond
    [(equal? (length l) (which-monsters sw)) '()]
    [else 
     (if (star-touch-monster? (rstar-posn (sw-rstar sw))
                              (monster-posn (first (level-monsters (first (sw-levels sw))))))
         (update-monster (make-sw (sw-rstar sw)
                                  (sw-score sw)
                                  (cons (make-level (level-stairs (first (sw-levels sw)))
                                                    (level-treats (first (sw-levels sw)))
                                                    (rest (level-monsters (first (sw-levels sw)))))
                                        (rest (sw-levels sw))))
                         (cons (make-monster (monster-type (first (level-monsters (first (sw-levels sw)))))
                                             (monster-posn (first (level-monsters (first (sw-levels sw)))))
                                             (- (monster-life
                                                 (first (level-monsters (first (sw-levels sw))))) 100))
                                                                      l))
         (cons (first (level-monsters (first (sw-levels sw))))
               (update-monster (make-sw (sw-rstar sw)
                                  (sw-score sw)
                                  (cons (make-level (level-stairs (first (sw-levels sw)))
                                                    (level-treats (first (sw-levels sw)))
                                                    (rest (level-monsters (first (sw-levels sw)))))
                                        (rest (sw-levels sw))))
                               (cons (first (level-monsters (first (sw-levels sw))))
                                                                      l))))]))

                
            

;update-monster-list: [List-of Monster] -> [List-of Monster]
;returns a new list of monsters
(define (update-monster-list monsters)
  (cond
    [(empty? monsters) '()]
    [else (if (equal? (monster-life (first monsters)) 0)
              (update-monster-list (rest monsters))
              (cons (first monsters) (update-monster-list (rest monsters))))]))

                                                   
;distance-stair-ceiling: sw -> num
;return a number represents the distance between stair and the ceiling
(define (distance-stair-ceiling sw)
  (- (posn-y (find-which-stair sw)) 5))
;test:
(check-expect (distance-stair-ceiling STARTING-SW) 135)

;star-jumping-update: rstar -> rstar
;return a star after jumping
(define (star-jumping-update star)
  (if (equal? 0 (rstar-jump star))
      (make-rstar (make-posn (posn-x (rstar-posn star))
                             (posn-y (rstar-posn star)))
                  (rstar-color star)
                  "falling"
                  0)
      (make-rstar (make-posn (posn-x (rstar-posn star))
                             (- (posn-y (rstar-posn star)) 5))
                  (rstar-color star)
                  "up"
                  (- (rstar-jump star) 1))))

;test:
(check-expect (star-jumping-update (make-rstar (make-posn 20 20) "black" "up" 2))
              (make-rstar (make-posn 20 15) "black" "up" 1))
(check-expect (star-jumping-update (make-rstar (make-posn 20 20) "black" "up" 0))
              (make-rstar (make-posn 20 20) "black" "falling" 0))


;update-world-after-touch: sw -> sw
;return a world after touch
(define (update-world-after-touch sw)
  (cond
    [(equal? (level-treats (first (sw-levels sw))) '()) sw]
    [(equal? (level-monsters (first (sw-levels sw))) '()) sw]
    [else 
     (if (star-touch-treat? (rstar-posn (sw-rstar sw))
                            (treat-posn (first (level-treats (first (sw-levels sw))))))
         (make-sw (update-star sw)
                  (sw-score sw)
                  (list (make-level (level-stairs (first (sw-levels sw)))
                                    (update-treat-list sw)
                                    (level-monsters (first (sw-levels sw))))))
         sw)]))

;update-satr: sw -> rstar
;returns a star
(define (update-star sw)
  (cond
    [(equal?  (level-treats (first (sw-levels sw))) '()) (sw-rstar sw)]
    [else (if (star-touch-treat? (rstar-posn (sw-rstar sw))
                                 (treat-posn (first (level-treats (first (sw-levels sw))))))
              (make-rstar (rstar-posn (sw-rstar sw))
                          (treat-color (first (level-treats (first (sw-levels sw)))))
                          (rstar-direction (sw-rstar sw))
                          (rstar-jump (sw-rstar sw)))
              (update-star (make-sw (sw-rstar sw)
                                    (sw-score sw)
                                    (cons (make-level (level-stairs (first (sw-levels sw)))
                                                      (rest (level-treats (first (sw-levels sw))))
                                                      (level-monsters (first (sw-levels sw))))
                                          (rest (sw-levels sw))))))]))
;test:
(check-expect (update-star (make-sw (make-rstar (make-posn 20 20)
                                                "red"
                                                "right"
                                                0)
                                    0                                    
                                    (list (make-level (list 20 20)
                                                      (list
                                                       (make-treat "red" (make-posn 200 100))
                                                       (make-treat "yellow" (make-posn 20 20)))
                                                      '()))))
              (make-rstar (make-posn 20 20)
                          "yellow"
                          "right"
                          0))


;flat-stair? sw -> bool
;return if the stair in the next is flat
(define (flat-stair? sw)
  (equal? (posn-y (find-which-stair sw))
          (posn-y (find-next-stair sw))))
;test:
(check-expect (flat-stair? STARTING-SW) #f)
(check-expect (flat-stair? TEST-SW) #f)


;update-star-posn: sw -> rstar
;return a new star
(define (update-star-posn sw)
  (cond
    [(equal? (rstar-direction (sw-rstar sw)) "up")
     (star-jumping-update (sw-rstar sw))]
    [(equal? (rstar-direction (sw-rstar sw)) "falling")
     (if (equal? (-(posn-y (find-which-stair sw)) 5)
                 (posn-y (rstar-posn (sw-rstar sw))))
         (make-rstar (rstar-posn (sw-rstar sw))
                     (rstar-color (sw-rstar sw))
                     "right"
                     0)
         (make-rstar (make-posn (posn-x (rstar-posn (sw-rstar sw)))
                                (+ (posn-y (rstar-posn (sw-rstar sw))) 5))
                     (rstar-color (sw-rstar sw))
                     "falling"
                     0))]
    [(equal? (rstar-direction (sw-rstar sw)) "left")
     (cond
       [(equal? (posn-x (rstar-posn (sw-rstar sw)))
                (- 10 (posn-x (find-which-stair sw))))
         (sw-rstar sw)]
       [else
        (cond
          [(and (equal? (rstar-jump (sw-rstar sw)) 0)
                (equal? (on-stair? sw) #f))
            (make-rstar (make-posn (- (posn-x (rstar-posn (sw-rstar sw))) 1)
                                   (posn-y (rstar-posn (sw-rstar sw))))
                        (rstar-color (sw-rstar sw))
                        "falling"
                        (rstar-jump (sw-rstar sw)))]
          [(and (equal? (rstar-jump (sw-rstar sw)) 0)
                (on-stair? sw))
           (make-rstar (make-posn (- (posn-x (rstar-posn (sw-rstar sw))) 1)
                                  (posn-y (rstar-posn (sw-rstar sw))))
                       (rstar-color (sw-rstar sw))
                       (rstar-direction (sw-rstar sw))
                       (rstar-jump (sw-rstar sw)))]
          [else (make-rstar (make-posn (- (posn-x (rstar-posn (star-jumping-update (sw-rstar sw)))) 1)
                                   (posn-y (rstar-posn (star-jumping-update (sw-rstar sw)))))
                        (rstar-color (sw-rstar sw))
                        (rstar-direction (star-jumping-update (sw-rstar sw)))
                        (rstar-jump (star-jumping-update (sw-rstar sw))))])])]
    [(equal? (rstar-direction (sw-rstar sw)) "right")
     (cond
       [(equal? (posn-x (rstar-posn (sw-rstar sw)))
                (+ 10 (posn-x (find-which-stair sw))))
         (sw-rstar sw)]
       [else
        (cond
          [(and (equal? (rstar-jump (sw-rstar sw)) 0)
                (equal? (on-stair? sw) #f))
            (make-rstar (make-posn (+ (posn-x (rstar-posn (sw-rstar sw))) 1)
                                   (posn-y (rstar-posn (sw-rstar sw))))
                        (rstar-color (sw-rstar sw))
                        "falling"
                        (rstar-jump (sw-rstar sw)))]
          [(and (equal? (rstar-jump (sw-rstar sw)) 0)
                (on-stair? sw))
           (make-rstar (make-posn (+ (posn-x (rstar-posn (sw-rstar sw))) 1)
                                  (posn-y (rstar-posn (sw-rstar sw))))
                       (rstar-color (sw-rstar sw))
                       (rstar-direction (sw-rstar sw))
                       (rstar-jump (sw-rstar sw)))]
          [else (make-rstar (make-posn (+ (posn-x (rstar-posn (star-jumping-update (sw-rstar sw)))) 1)
                                   (posn-y (rstar-posn (star-jumping-update (sw-rstar sw)))))
                        (rstar-color (sw-rstar sw))
                        (rstar-direction (star-jumping-update (sw-rstar sw)))
                        (rstar-jump (star-jumping-update (sw-rstar sw))))])])]))
;test
(check-expect (update-star-posn (make-sw (make-rstar (make-posn 20 30) "red" "up" 1)
                                         0
                                         (list LEVEL-ONE)))
              (make-rstar (make-posn 20 25) "red" "up" 0))
(check-expect (update-star-posn (make-sw (make-rstar (make-posn 20 30) "red" "falling" 0)
                                         0
                                         (list LEVEL-ONE)))
              (make-rstar (make-posn 20 35) "red" "falling" 0))
(check-expect (update-star-posn (make-sw (make-rstar (make-posn 20 135) "red" "left" 0)
                                         0
                                         (list LEVEL-ONE)))
              (make-rstar (make-posn 19 135) "red" "left" 0))
(check-expect (update-star-posn (make-sw (make-rstar (make-posn 20 20) "red" "left" 2)
                                         0
                                         (list LEVEL-ONE)))
              (make-rstar (make-posn 19 15) "red" "up" 1))
(check-expect (update-star-posn (make-sw (make-rstar (make-posn 20 20) "red" "right" 2)
                                         0
                                         (list LEVEL-ONE)))
              (make-rstar (make-posn 21 15) "red" "up" 1))


  
;key: SW Key-event -> SW
;Builds a key event to control the paddle
;Strategy: function composition
(define (key sw a-key)
  (cond
    [(key=? a-key " ")
     (make-sw (make-rstar (rstar-posn (sw-rstar sw))
                          (rstar-color (sw-rstar sw))
                          "up"
                          (+ (rstar-jump (sw-rstar sw)) 3))
              (sw-score sw)
              (sw-levels sw))]
    [(key=? a-key "left") (make-sw (make-rstar (rstar-posn (sw-rstar sw))
                                            (rstar-color (sw-rstar sw))
                                            "left"
                                             (rstar-jump (sw-rstar sw)))
                                   (sw-score sw)
                                   (sw-levels sw))]
    [(key=? a-key "right") (make-sw (make-rstar (rstar-posn (sw-rstar sw))
                                            (rstar-color (sw-rstar sw))
                                            "right"
                                             (rstar-jump (sw-rstar sw)))
                                    (sw-score sw)
                                    (sw-levels sw))]
    [else sw]))
;tests
(check-expect (key (make-sw (make-rstar (make-posn 2 2) "Oliver" "left" 0)
                                        0
                                        (list LEVEL-ONE))
                   " ")
              (make-sw (make-rstar (make-posn 2 2) "Oliver" "up" 3)
                                        0
                                        (list LEVEL-ONE)))


                                                                    

;on-stair?: sw -> bool
;returns a bool to check if the star is on the stair
(define (on-stair? sw)
  (equal? -5 (- (posn-y (rstar-posn(sw-rstar sw)))
             (posn-y (find-which-stair sw)))))

;test:
(check-expect (on-stair? STARTING-SW) #t)
(check-expect (on-stair? TEST-SW) #t)

;violate-colors?: sw -> bool
;returns a boolean to check if it against the color rule
(define (violate-colors? sw)
  (cond
    [(equal? (level-treats (first (sw-levels sw))) '()) #f]
    [else  (and (star-touch-treat? (rstar-posn (sw-rstar sw))
                                   (treat-posn (first (level-treats (first (sw-levels sw))))))
                (or (and (equal? (rstar-color (sw-rstar sw)) "coral")
                         (equal? (treat-color (first (level-treats (first (sw-levels sw))))) "blue"))
                    (and (equal? (rstar-color (sw-rstar sw)) "blue")
                         (equal? (treat-color (first (level-treats (first (sw-levels sw))))) "coral"))))]))
;violate-monsters?: sw->boolean
;returns a boolean to see if it against the monster rule
(define (violate-monsters? sw)
  (cond
    [(equal? (level-monsters (first (sw-levels sw))) '()) #f]
    [else (and (equal? (star-touch-monster? (rstar-posn (sw-rstar sw))
                                    (monster-posn (first (level-monsters (first (sw-levels sw)))))) #f)
               (equal? (posn-x (rstar-posn (sw-rstar sw)))
                       (- (posn-x (monster-posn (first (level-monsters (first (sw-levels sw)))))) 8))
               (on-stair? sw))]))
    

;end-game?: sw -> bool
;returns to see if the game end
(define (end-game? sw)
  (or
   (<= (posn-x (rstar-posn (sw-rstar sw))) 0)
   (>= (posn-y (rstar-posn (sw-rstar sw))) WORLD-HEIGHT)
   (<= (posn-y (rstar-posn (sw-rstar sw))) 0)
   (violate-colors? sw)
   (violate-monsters? sw)))
      
;test:
(check-expect (end-game? STARTING-SW) #f)



;end-level: SW -> Boolean
;returns a boolean to see if its the end of the level
(define (end-level? sw)
 (> (posn-x (rstar-posn (sw-rstar sw)))
    (- WORLD-WIDTH 10)))
     
;tests
(check-expect (end-level? STARTING-SW) #f)
(check-expect (end-level? (make-sw (make-rstar (make-posn 795 0)
                                              "red"
                                              "up"
                                              0)
                                  0
                                  (cons LEVEL-ONE '()))) #t)

;end-world: sw -> sw
;return an ending world
(define (end-world sw)
  (make-sw (make-rstar (make-posn 0 0) "red" "up" 0)
              (sw-score sw)
              (sw-levels sw)))
              

;tick: SW -> SW
(define (tick sw)
  (cond
    [(end-game? sw) (end-world sw)]
    [(end-level? sw) (make-sw (make-rstar (make-posn 10 80) "black" "right" 0)
                              (sw-score sw)
                              (rest (sw-levels sw)))]
    [else (update-world sw)]))


;update-world: SW -> SW
;update the world after tick
(define (update-world sw)
  (local
    [(define NSW
       (update-world-after-touch sw))]
    (make-sw (update-star (make-sw (update-star-posn NSW)
                                   (sw-score NSW)
                                   (sw-levels NSW)))
             (sw-score NSW)
             (update-levels NSW))))
;tests:


;update-levels: SW -> [List-Of Level]
;update the list of level of the world
(define (update-levels sw)
  (cons (make-level (level-stairs (first (sw-levels sw)))
                    (update-treat-list sw)
                    (update-monster-list (update-monster sw '() (length (level-monsters (first (sw-levels sw)))))))
        (rest (sw-levels sw))))



;place-image/posn: Image Point Image -> Imgae
;returns a image with a image on it in certain point
(define (place-image/posn im posn bg)
  (place-image im (posn-x posn) (posn-y posn) bg))
;tests:
(check-expect (place-image/posn STAIR-IMAGE (make-posn 50 50) BACKGROUND)
              (place-image STAIR-IMAGE 50 50 BACKGROUND))


;draw-staris: SW -> Imgae
;returns a image with all the stairs
(define (draw-stairs sw)
  (local
    [(define ST
       (level-stairs (first (sw-levels sw))))]
    (cond
      [(empty? ST) BACKGROUND]
      [else (if (equal? (make-posn 0 0)
                        (rstar-posn (sw-rstar sw)))
                (place-image (text "GAME OVER!!!" 80 "red") 400 100 BACKGROUND)
                (place-image/posn STAIR-IMAGE (first ST)
                              (draw-stairs (make-sw
                                            (sw-rstar sw)
                                            (sw-score sw)
                                            (cons (make-level (rest ST) (level-treats (first (sw-levels sw)))
                                                              (level-monsters (first (sw-levels sw))))
                                                  (rest (sw-levels sw)))))))])))

;draw-treats: SW -> Imgae
;returns a imgae with all the treats
(define (draw-treats-stairs sw)
  (local
    [(define TT
       (level-treats (first (sw-levels sw))))]
    (cond
      [(empty? TT) (draw-stairs sw)]
      [else (place-image/posn (make-treat-image (first TT))
                              (treat-posn (first TT))
                              (draw-treats-stairs (make-sw
                                                   (sw-rstar sw)
                                                   (sw-score sw)
                                                   (cons (make-level (level-stairs (first (sw-levels sw)))
                                                                     (rest TT)
                                                                     (level-monsters (first (sw-levels sw))))
                                                         (rest (sw-levels sw))))))])))

;draw-rstar: SW -> Image
;returns a image
(define (draw-rstar sw)
  (place-image/posn (make-star-image (sw-rstar sw))
                    (rstar-posn (sw-rstar sw))
                    (draw-treats-stairs sw)))

;draw-monsters :sw-> image
;return a image
(define (draw-monsters sw)
  (cond
    [(empty? (level-monsters (first (sw-levels sw)))) (draw-rstar sw)]
    [else (place-image/posn (monster-type (first (level-monsters (first (sw-levels sw)))))
                            (monster-posn (first (level-monsters (first (sw-levels sw)))))
                            (draw-monsters (make-sw
                                            (sw-rstar sw)
                                            (sw-score sw)
                                            (cons (make-level (level-stairs (first (sw-levels sw)))
                                                              (level-treats (first (sw-levels sw)))
                                                              (rest (level-monsters (first (sw-levels sw)))))
                                                  (rest (sw-levels sw))))))]))

; draw: Faller-World -> Image
; Draws the score on the top of the scene
; Strategy: Function Composition
(define (draw sw)
  (place-image 
   (text (number->string (update-score sw)) 15 "olive")
   15 15
   (draw-monsters sw)))
;which-level: sw -> num
;returns a num to represent which level it is
(define (which-level sw)
  (length (sw-levels sw)))
;tests
(check-expect (which-level STARTING-SW) 4)

;which-treats: sw -> length
;return the length of a list of treats
(define (which-treats sw)
  (cond
    [(equal? (which-level sw) 4) (length LEVEL-ONE-TREATS)]
    [(equal? (which-level sw) 3) (length LEVEL-TWO-TREATS)]
    [(equal? (which-level sw) 2) (length LEVEL-THREE-TREATS)]
    [(equal? (which-level sw) 1) (length LEVEL-FOUR-TREATS)]))

;which-monsters: sw -> length
;return the length of a list of monsters
(define (which-monsters sw)
  (cond
    [(equal? (which-level sw) 4) 0]
    [(equal? (which-level sw) 3) (length LEVEL-TWO-MONSTER)]
    [(equal? (which-level sw) 2) (length LEVEL-THREE-MONSTER)]
    [(equal? (which-level sw) 1) (length LEVEL-FOUR-MONSTER)]))

;update-score: SW -> Num
;returns the updated score
(define (update-score sw)
  (local
    [(define CT
       (length (level-treats (first (update-levels sw)))))]
    (+ (+ (sw-score sw) (* (- (which-treats sw) CT) 10))
       (* (- (which-monsters sw) (length (level-monsters
                                          (first (update-levels sw))))) 100))))

;start: Any -> SW
;Starts the star sunning game
;example:
(define (start _dummy)
  (big-bang TEST-SW-F
    [on-tick tick (/ 1 10)]
    [on-key key]
    [to-draw draw]))


;(start 0)

