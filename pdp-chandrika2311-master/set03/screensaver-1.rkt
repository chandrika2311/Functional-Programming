;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; start with (main 0)

(require rackunit)
(require "extras.rkt")
(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         new-circle
         circ-x
         circ-y
         circ-vx
         circ-vy)
(require 2htdp/universe)
(require 2htdp/image)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SCREENSAVER FUNCTION.

;;screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; circles paused
;; RETURNS: the final state of the world
(define (screensaver initial-pos1)
  (big-bang (initial-world initial-pos1)
            (on-tick world-after-tick 0.5 )
            (on-key world-after-key-event)
            (on-draw world-to-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;;Canvas dimensions can be constant 
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct circ (x y vx vy))
;;A Circ is a
;;(make-circ Integer Integer Integer Integer)
;;Interpretation:
;;x: x-coordinate of circle in pixel
;;y: y-coordinate of circle
;;vx: velocity of circle in x direction
;;vy: velocity of circle in y direction
;;TEMPLATE:
;;circ-fn: Circ -> ??
;;
;(define (circ-fn c)
;  (... ((circ-x c)(circ-y c)(circ-vx)(circ-vx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct worldstate (circle1 circle2 paused?))
;; A WorldState is a
;; (make-worldstate circle1 clrcle2 Boolean)
;; Interpretation: 
;; circle1: is the second circle
;; circle2: is the first circle
;; paused?: describes whether or not the circles are paused.
;; TEMPLATE
;; worldstate-fn : WorldState -> ??
;;(define (worldstate-fn w)
;;  (... (worldstate-circle1 w) (worldstate-circle2 w) (worldstate-paused? w)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions related to world:
;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world where circles centered at (200,100) and (200,200) are paused
;; STRATEGY : COMBINE SIMPLER FUNCTIONS
;; EXAMLPLES:
;; (initial-world 2) => (make-worldstate (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t )
;; (initial-world A) => (make-worldstate (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t )
;; (initial-world 3) => (make-worldstate (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t )
(define (initial-world x)
  (make-worldstate (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t )
  )
(begin-for-test
  (check-equal?
   (initial-world 2)(make-worldstate (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t ) "(make-worldstate (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t ) expected")
   (check-equal?(initial-world 3)
                (make-worldstate (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t ) "(make-worldstate (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t ) expected"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-tick : WorldState -> WorldState
;; GIVEN : WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; STRATEGY : Use template for WorldState on w
;;EXAMPLES:
;;(world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t)=> (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t)
;;(world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f)=> (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f)
;; 
(define (world-after-tick w)
  (if(worldstate-paused? w)
     w
     (unpaused-world-after-tick w)))

;; tests:
(begin-for-test
  (check-equal?
    (world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t))
    (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t)
    "(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t) expected"))
(begin-for-test
  (check-equal?
    (world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f))
    (unpaused-world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f))
    "(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f) expected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unpaused-world-after-tick : WorldState -> WorldState
;; GIVEN : WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick when unpaused.
;; STRATEGY : Use template for WorldState on w
;;Examples:
;;(unpaused-world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f)) =>(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f
(define (unpaused-world-after-tick w)
  (make-worldstate(calculate_new_circle_dimensions (worldstate-circle1 w))
             (calculate_new_circle_dimensions (worldstate-circle2 w))
             #f ))
;; tests:
(begin-for-test
  (check-equal?
    (unpaused-world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f))
    (make-worldstate (calculate_new_circle_dimensions (new-circle 200 100 -12 20))(calculate_new_circle_dimensions (new-circle 200 200 23 -14)) #f)
    "(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f) expected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functin for world-after-tick
;; calculate_new_circle_dimensions : Circ -> Circ
;; GIVEN : Circ
;; RETURNS: the circ state that should follow the after tick
;; after a tick when unpaused.
;; STRATEGY : Use template for Circ on c
;;EXAMPLES: (calculate_new_circle_dimensions (new-circle 200 100 -12 20))=>(make-circ 188 120 -12 20)
;;          (calculate_new_circle_dimensions (new-circle 200 200 23 -14))=>(make-circ 223 186 23 -14)
;;          (calculate_new_circle_dimensions (new-circle 39 200 23 -14))=> (make-circ 16 186 23 -14)
;           (calculate_new_circle_dimensions (new-circle 361 360 23 -14))=>(make-circ 338 360 23 -14)
;;          (calculate_new_circle_dimensions (new-circle 30 30 -23 -14))=> (make-circ 63 54 23 14)))
;;          (calculate_new_circle_dimensions (new-circle 360 260 23 14))=> (make-circ 337 246 -23 -14)))
;;          (calculate_new_circle_dimensions (new-circle 30 270 -20 10))=> (make-circ 60 250 20 -10)))
;;          (calculate_new_circle_dimensions (new-circle 380 30 20 -10))=> (make-circ 340 50 -20 10)))
;;
(define (calculate_new_circle_dimensions c)
  (cond
    [(and(and (>(circ-x c)  40) (< (circ-x c) 360))
         (and (>(circ-y c) 40) (<(circ-y c) 260)))
         (make-circ (circle_new_position(circ-x c)(circ-vx c))(circle_new_position(circ-y c)(circ-vy c)) (circ-vx c) (circ-vy c))]
    [(and(<= (circ-x c) 40)
         (and ( > (circ-y c) 40) (< (circ-y c) 260)))(make-circ (circle_new_position 40 (* -1 (circ-vx c)))
                                                                (circle_new_position(circ-y c)(circ-vy c))(* -1 (circ-vx c))(circ-vy c))]
     
    [(and ( >= (circ-x c) 360) 
          (and (> (circ-y c) 40) ( < (circ-y c) 260)))(make-circ(circle_new_position 360 (* -1 (circ-vx c)))
                                                                (circle_new_position(circ-y c)(circ-vy c))  (* -1(circ-vx c)) (circ-vy c))]
    
    [(and(and (> (circ-x c)  40) ( < (circ-x c) 360))
         ( <= (circ-y c) 40) )(make-circ(circle_new_position(circ-x c)(circ-vx c)) (circle_new_position 40 (* -1(circ-vy c)))   (circ-vx c)(* -1(circ-vy c)))]
    
    [(and(and (>(circ-x c)  40) (<(circ-x c) 360)) 
         (>=(circ-y c) 260) )(make-circ (circle_new_position(circ-x c)(circ-vx c))(circle_new_position 260 (* -1(circ-vy c))) (circ-vx c) (* -1(circ-vy c)))]
    
    [(and(<= (circ-x c) 40)
         (<= (circ-y c) 40))(make-circ (circle_new_position 40 (* -1 (circ-vx c)))
                                       (circle_new_position 40 (* -1(circ-vy c))) (* -1 (circ-vx c)) (* -1(circ-vy c)))]
    [(and(>= (circ-x c) 360)
         (>= (circ-y c) 260)) (make-circ (circle_new_position 360 (* -1 (circ-vx c)))
                                         (circle_new_position 260 (* -1(circ-vy c))) (* -1 (circ-vx c)) (* -1(circ-vy c)))]
    [(and(<= (circ-x c) 40)
         (>= (circ-y c) 260))(make-circ (circle_new_position 40 (* -1 (circ-vx c)))
                                         (circle_new_position 260 (* -1(circ-vy c))) (* -1 (circ-vx c)) (* -1(circ-vy c)))]
    [(and(>= (circ-x c) 360)
         (<= (circ-y c) 40))(make-circ (circle_new_position 360 (* -1 (circ-vx c)))
                                         (circle_new_position 40 (* -1(circ-vy c))) (* -1 (circ-vx c)) (* -1(circ-vy c)))]))

;...................;...................;...................;...................;...................;...................
;;Tests:


(begin-for-test
  (check-equal?(calculate_new_circle_dimensions (new-circle 40 200 23 -14))(make-circ 17 186 -23 -14)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 360 200 23 -14))(make-circ 337 186 -23 -14)))

(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 50 30 -23 -14))(make-circ 27 54 -23 14)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 50 260 -23 -14))(make-circ 27 274 -23 14)))

(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 30 30 -23 -14))(make-circ 63 54 23 14)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 360 260 23 14))(make-circ 337 246 -23 -14)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 30 270 -20 10))(make-circ 60 250 20 -10)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 380 30 20 -10))(make-circ 340 50 -20 10)))
;...................;...................;...................;...................;...................;...................

;;Circle_new_position : c v -> Integer
;; GIVEN : coordinate velocity
;; RETURNS: Integer
;; STRATEGY : combining simpler functions
;;EXAMPLES:
;;(circle_new_position 3 -14) => -11
;;(circle_new_position 3 14) => 17
;;(circle_new_position 50 4) => 54 

 (define (circle_new_position coordinate velocity)
    ( + coordinate velocity))

(begin-for-test
  (check-equal?(circle_new_position 360 -23) 337))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN:
;;       WorldState: current world state
;;       KeyEvent  : input of space bar
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; STRATEGY: cases on kev : KeyEvent
;;
(define (world-after-key-event w kev)
  (if (is-pause-key-event? kev)
    (world-with-paused-toggle w)
    w)
  )


;; help function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
(define (is-pause-key-event? ke)
  (key=? ke " "))

;;Tests:
(begin-for-test
  (check-equal?(is-pause-key-event? " ") #t))

 
;;world-with-paused-toggle: World -> World
;; RETURNS: a world like the given one, but with paused? toggled
;;STRATEGY: Use template for world on w
;;EXAMPLES: (world-with-paused-toggle(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f))
;;=>(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t)

(define (world-with-paused-toggle w)
  (
   make-worldstate (worldstate-circle1 w) (worldstate-circle2 w) (not (worldstate-paused? w)))
   )
(begin-for-test
  (check-equal?(world-with-paused-toggle(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f))
               (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy).
;;STRATEGY: combine simpler functions
;;EXAMPLES:(new-circle 100 200 23 23) =>(make-circ 100 200 23 23)


(define (new-circle x y vx vy)
  (make-circ x y vx vy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene (make-worldstate 200 200 ??))
;;          = (place-image CAT-IMAGE CAT-X-COORD 20 EMPTY-CANVAS)
;; STRATEGY: Use template for World on w



(define (world-to-scene w)
  (place-images (list (circle 40 "outline" "blue")
                     (circle 40 "outline" "blue")
                     (text (string-append (number->string(circ-vx (worldstate-circle1 w)))
                                                           ","
                                           (number->string(circ-vy (worldstate-circle1 w)))) 10 "red")
                     (text (string-append (number->string(circ-vx (worldstate-circle2 w)))
                                             ","
                                             (number->string(circ-vy (worldstate-circle2 w)))) 10 "red"))
               (list (make-posn (circ-x (worldstate-circle1 w)) (circ-y (worldstate-circle1 w)))
                     (make-posn (circ-x (worldstate-circle2 w)) (circ-y (worldstate-circle2 w)))
                     (make-posn (circ-x (worldstate-circle1 w)) (circ-y (worldstate-circle1 w)))
                     (make-posn (circ-x (worldstate-circle2 w)) (circ-y (worldstate-circle2 w))))
               (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))
                    
;; tests







