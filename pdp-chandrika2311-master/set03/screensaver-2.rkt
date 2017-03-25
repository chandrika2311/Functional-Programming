;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; start with (main 0)

(require rackunit)
(require "extras.rkt")
(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         new-circle
         circ-x
         world-after-mouse-event
         circ-after-mouse-event
         circ-selected?
         circ-y
         circ-vx
         circ-vy)
(check-location "03" "screensaver-2.rkt")
(require 2htdp/universe)
(require 2htdp/image)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SCREENSAVER FUNCTION.

;;screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver initial-pos1)
  (big-bang (initial-world initial-pos1)
            (on-tick world-after-tick 1 )
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;;Canvas dimensions can be constant 
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct circ (x y vx vy selected? storedx storedy))
;;A circle is a
;;(make-circl Integer Integer Integer Integer Boolean Integer Integer)
;;Interpretation:
;;R :Radius
;;x: x-coordinate of circle in pixel
;;y: y-coordinate of circle
;;vx: velocity of circle in x direction
;;vy: velocity of circle in y direction
;;selected?: true if circle selected
;;storedx: stores dx value
;;storedy: stores dy value

;TEMPLATE:
;;;;circle-fn: circle -> ??
;;;;
;;;;(define (circle-fn c)
;;;;  (... ((circ-x c)(circ-x c)(circ-vx c)(circ-vy c)(circ-selected? c)(circ-storedx c)(circ-storedy c))
;; examples of circles, for testing
(define selected-circle1-at-200-100 (make-circ 200 100 -12 20 true 0 0))
(define unselected-circle4-at-200-200 (make-circ 200 200 23 -14 false 0 0))

(define selected-circle2-at-200-100 (make-circ 30 40 -12 20 true 0 0 ))
(define unselected-circle3-at-200-200 (make-circ 30 50 23 -14 false 0 0))

(define selected-circle5-at-200-100 (make-circ 10 100 -12 20 true 0 0))
(define unselected-circle6-at-200-200 (make-circ 20 20 23 -14 false 0 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct worldstate (circle1 circle2 paused? reddot))
;; A WorldState is a (make-worldstate circle1 clrcle2 Boolean reddot)
;; Interpretation: 
;; circle1: is the second circle
;; circle2: is the first circle
;; paused?: describes whether or not the circles are paused.
;; TEMPLATE
;; worldstate-fn : WorldState -> ??

;;(define (worldstate-fn w)
;;  (... (worldstate-circle1 w) (worldstate-circle2 w) (worldstate-paused? w)(worldstate-reddot w))
(define-struct reddot(x y clicked?))
;; A RedDot is a (make-worldstate x y Boolean)
;; Interpretation: 
;; x: is the x axis
;; y: is the y axis
;; clicked?: describes whether or not the mouse is clicked.
;; TEMPLATE
;; reddot-fn : Reddot -> ??

;;(define (reddot-fn r)
;;  (... (reddot-x r) (reddot-y r) (reddot-clicked? r))  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A Mouseevent is one of:
;;--"button-down"
;;--"drag"
;;--"button-up"

;mev-fn : MouseEvent -> ??
;STRATEGY: Cases on MouseEvent mev
;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [else ...]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions related to world:
;;initial-world : Any -> WorldState
;; GIVEN:   Any value (ignored)
;; RETURNS: The initial world specified in the problem set
;;STRATEGY: Combining simpler functions
;;EXAMPLES:
;; (initial-world 1) =>(make-worldstate(new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t (reddot 0 0 #f))
;; (initial-world 2) =>(make-worldstate(new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t #f)

(define (initial-world x)
  (make-worldstate (new-circle 200 100 -12 20) (new-circle 200 200 23 -14)  #t (make-reddot 0 0 #f))
  ) 
;; TESTS
(begin-for-test
  (check-equal? (initial-world 1) (make-worldstate(new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t (make-reddot 0 0 #f) )
    "(make-worldstate(new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t #f) expected")
  (check-equal? (initial-world 5) (make-worldstate(new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t (make-reddot 0 0 #f) )
    "(make-worldstate(new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #t) expected "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-tick : WorldState -> WorldState
;; GIVEN : WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; STRATEGY : Use template for WorldState on w
;;EXAMPLES:
;;(world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t)
;;=> (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t)
;;(world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f)
;;=> (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f)

(define (world-after-tick w)
  (if(worldstate-paused? w)
     w
     (unpaused-world-after-tick w)))
;; tests:
(begin-for-test
  (check-equal?
    (world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t (make-reddot 0 0 #f)))
    (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t (make-reddot 0 0 #f))
    "(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t (make-reddot 0 0 #f)) expected"))
(begin-for-test
  (check-equal?
    (world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f (make-reddot 0 0 #f)))
    (unpaused-world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f (make-reddot 0 0 #f)))
    "(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f) expected"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unpaused-world-after-tick : WorldState -> WorldState
;; GIVEN : WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick when unpaused.
;; STRATEGY : Use template for WorldState on w
;;Examples:
;;(unpaused-world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f))
;;=>(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f

(define (unpaused-world-after-tick w)
  (make-worldstate(calculate_new_circle_dimensions (worldstate-circle1 w))
             (calculate_new_circle_dimensions (worldstate-circle2 w))
             #f (worldstate-reddot w)))
;; tests:
(begin-for-test
  (check-equal?
    (unpaused-world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f (make-reddot 0 0 #f)))
    (make-worldstate (calculate_new_circle_dimensions (new-circle 200 100 -12 20))(calculate_new_circle_dimensions (new-circle 200 200 23 -14)) #f (make-reddot 0 0 #f))
    "(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f) expected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         
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
(define (calculate_new_circle_dimensions c)
  (cond
    [(and(and (>(circ-x c)  40) (< (circ-x c) 360))
         (and (>(circ-y c) 40) (<(circ-y c) 260)))
         (make-circ (circle_new_position(circ-x c)(circ-vx c))(circle_new_position(circ-y c)(circ-vy c)) (circ-vx c) (circ-vy c)(circ-selected? c)
                    (circ-storedx c) (circ-storedy c))]
    [(and(<= (circ-x c) 40)
         (and ( > (circ-y c) 40) (< (circ-y c) 260)))(make-circ (circle_new_position 40 (* -1 (circ-vx c)))
                                                                (circle_new_position(circ-y c)(circ-vy c))(* -1 (circ-vx c))(circ-vy c)(circ-selected? c)(circ-storedx c)
                                                                (circ-storedy c))]
     
    [(and ( >= (circ-x c) 360) 
          (and (> (circ-y c) 40) ( < (circ-y c) 260)))(make-circ(circle_new_position 360 (* -1 (circ-vx c)))
                                                                (circle_new_position(circ-y c)(circ-vy c))
                                                                (* -1(circ-vx c)) (circ-vy c)(circ-selected? c) (circ-storedx c) (circ-storedy c))]
    
    [(and(and (> (circ-x c)  40) ( < (circ-x c) 360))
         ( <= (circ-y c) 40) )(make-circ(circle_new_position(circ-x c)(circ-vx c)) (circle_new_position 40 (* -1(circ-vy c)))(circ-vx c)(* -1(circ-vy c))
                                        (circ-selected? c) (circ-storedx c) (circ-storedy c))]
    
    [(and(and (>(circ-x c)  40) (<(circ-x c) 360)) 
         (>=(circ-y c) 260) )(make-circ (circle_new_position(circ-x c)(circ-vx c))(circle_new_position 260 (* -1(circ-vy c)))(circ-vx c)
                                        (* -1(circ-vy c))(circ-selected? c) (circ-storedx c) (circ-storedy c))]
    
    [(and(<= (circ-x c) 40)
         (<= (circ-y c) 40))(make-circ (circle_new_position 40 (* -1 (circ-vx c)))
                                       (circle_new_position 40 (* -1(circ-vy c))) (* -1 (circ-vx c)) (* -1(circ-vy c))(circ-selected? c) (circ-storedx c) (circ-storedy c))]
    [(and(>= (circ-x c) 360)
         (>= (circ-y c) 260)) (make-circ (circle_new_position 360 (* -1 (circ-vx c)))
                                         (circle_new_position 260 (* -1(circ-vy c))) (* -1 (circ-vx c)) (* -1(circ-vy c))(circ-selected? c)(circ-storedx c) (circ-storedy c))]
    [(and(<= (circ-x c) 40)
         (>= (circ-y c) 260))(make-circ (circle_new_position 40 (* -1 (circ-vx c)))
                                         (circle_new_position 260 (* -1(circ-vy c))) (* -1 (circ-vx c)) (* -1(circ-vy c))(circ-selected? c)(circ-storedx c) (circ-storedy c))]
    [(and(>= (circ-x c) 360)
         (<= (circ-y c) 40))(make-circ (circle_new_position 360 (* -1 (circ-vx c)))
                                         (circle_new_position 40 (* -1(circ-vy c))) (* -1 (circ-vx c)) (* -1(circ-vy c)) (circ-selected? c) (circ-storedx c) (circ-storedy c))]))

;...................;...................;...................;...................;...................;...................
;;Tests:
(begin-for-test
  (check-equal?(calculate_new_circle_dimensions (new-circle 40 200 23 -14))(make-circ 17 186 -23 -14 #f 0 0)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 360 200 23 -14))(make-circ 337 186 -23 -14 #f 0 0)))

(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 50 30 -23 -14))(make-circ 27 54 -23 14 #f 0 0)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 50 260 -23 -14))(make-circ 27 274 -23 14 #f 0 0)))

(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 30 30 -23 -14))(make-circ 63 54 23 14 #f 0 0)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 360 260 23 14))(make-circ 337 246 -23 -14 #f 0 0)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 30 270 -20 10))(make-circ 60 250 20 -10 #f 0 0)))
(begin-for-test
(check-equal?(calculate_new_circle_dimensions (new-circle 380 30 20 -10))(make-circ 340 50 -20 10 #f 0 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN:
;;       WorldState: current world state
;;       KeyEvent  : input of space bar
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; STRATEGY: cases on kev : KeyEvent
(define (world-after-key-event w kev)
  (if (is-pause-key-event? kev)
    (world-with-paused-toggle w)
    w)
  )


;; help function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
;; STRATEGY: cases on kev : KeyEvent
(define (is-pause-key-event? ke)
  (key=? ke " "))
;;Tests:
(begin-for-test
  (check-equal?(is-pause-key-event? " ") #t))

 
;;world-with-paused-toggle: WorldState -> WorldState
;;GIVEN: WorldState
;;RETURNS: a world like the given one, but with paused? toggled
;;STRATEGY: Use template for world on w

(define (world-with-paused-toggle w)
  ( make-worldstate(worldstate-circle1 w) (worldstate-circle2 w) (not (worldstate-paused? w)) (worldstate-reddot w))
   )
(begin-for-test
  (check-equal?(world-with-paused-toggle(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f (make-reddot 0 0 #f) ))
               (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #t (make-reddot 0 0 #f))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy).

(define (new-circle x y vx vy)
  (make-circ x y vx vy #f 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: WorldState
;; RETURNS: a Scene that portrays the given worldstate.
;; EXAMPLE: (world-to-scene (make-worldstate- 200 200 ??))
;;          = (place-image circl-IMAGE circl-X-COORD 20 EMPTY-CANVAS)
;; STRATEGY: Use template for worldstate on w

(define (world-to-scene w)
  (cond
    [ (and (circ-selected? (worldstate-circle1 w))
           (not(circ-selected? (worldstate-circle2 w)))
           (reddot-clicked? (worldstate-reddot w))) (place-images (list
                                                                   (circle 5 "solid" "red") 
                                                                   (circle 40 "outline" "red")
                                                                   (circle 40 "outline" "blue")
                                                                   (text (string-append (number->string(circ-vx (worldstate-circle1 w)))
                                                                                        ","
                                                                                        (number->string(circ-vy (worldstate-circle1 w)))) 10 "red")
                                                                   (text (string-append (number->string(circ-vx (worldstate-circle2 w)))
                                                                                        ","
                                                                                        (number->string(circ-vy (worldstate-circle2 w)))) 10 "red"))
                                                                  
                                                                  (list
                                                                   (make-posn (reddot-x   (worldstate-reddot w)) (reddot-y   (worldstate-reddot w)))
                                                                   (make-posn (circ-x (worldstate-circle1 w))(circ-y (worldstate-circle1 w)))
                                                                   (make-posn (circ-x (worldstate-circle2 w))(circ-y (worldstate-circle2 w)))
                                                                   (make-posn (circ-x (worldstate-circle1 w))(circ-y (worldstate-circle1 w)) )
                                                                   (make-posn (circ-x (worldstate-circle2 w))(circ-y (worldstate-circle2 w)) ))
                                                                  (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)) ]
    [ (and (circ-selected? (worldstate-circle2 w))
           (not(circ-selected? (worldstate-circle1 w)))
           (reddot-clicked? (worldstate-reddot w)) )               (place-images
                                                                    (list 
                                                                     (circle 5 "solid" "red")
                                                                     (circle 40 "outline" "blue")
                                                                     (circle 40 "outline" "red")
                                                                     (text (string-append (number->string(circ-vx (worldstate-circle1 w)))
                                                                                          ","
                                                                                          (number->string(circ-vy (worldstate-circle1 w)))) 10 "red")
                                                                     (text (string-append (number->string(circ-vx (worldstate-circle2 w)))
                                                                                          ","
                                                                                          (number->string(circ-vy (worldstate-circle2 w)))) 10 "red"))
                                                                    (list
                                                                     (make-posn (reddot-x   (worldstate-reddot w)) (reddot-y   (worldstate-reddot w)))
                                                                     (make-posn (circ-x (worldstate-circle1 w)) (circ-y (worldstate-circle1 w)))
                                                                     (make-posn (circ-x (worldstate-circle2 w)) (circ-y (worldstate-circle2 w)))
                                                                     (make-posn (circ-x (worldstate-circle1 w))(circ-y (worldstate-circle1 w)) )
                                                                     (make-posn (circ-x (worldstate-circle2 w))(circ-y (worldstate-circle2 w)) ))
                                                                    (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)) ]
    [ (and (circ-selected? (worldstate-circle2 w)) 
           (circ-selected? (worldstate-circle1 w))
           (reddot-clicked? (worldstate-reddot w)))                (place-images (list
                                                                   (circle 5 "solid" "red")
                                                                   (circle 40 "outline" "red")
                                                                   (circle 40 "outline" "red")
                                                                   (text (string-append (number->string(circ-vx (worldstate-circle1 w)))
                                                                                        ","
                                                                                        (number->string(circ-vy (worldstate-circle1 w)))) 10 "red")
                                                                   (text (string-append (number->string(circ-vx (worldstate-circle2 w)))
                                                                                        ","
                                                                                        (number->string(circ-vy (worldstate-circle2 w)))) 10 "red"))
                                                                  (list
                                                                   (make-posn (reddot-x   (worldstate-reddot w)) (reddot-y   (worldstate-reddot w)))
                                                                   (make-posn (circ-x (worldstate-circle1 w)) (circ-y (worldstate-circle1 w)))
                                                                   (make-posn (circ-x (worldstate-circle2 w)) (circ-y (worldstate-circle2 w)))
                                                                   (make-posn (circ-x (worldstate-circle1 w))(circ-y (worldstate-circle1 w)) )
                                                                   (make-posn (circ-x (worldstate-circle2 w))(circ-y (worldstate-circle2 w)) ))
                                                                  (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)) ] 
    [ (reddot-clicked? (worldstate-reddot w)) (place-images (list 
                                                             (circle 5 "solid" "red")
                                                             (circle 40 "outline" "blue")
                                                             (circle 40 "outline" "blue")
                                                             (text (string-append (number->string(circ-vx (worldstate-circle1 w)))
                                                                                  ","
                                                                                  (number->string(circ-vy (worldstate-circle1 w)))) 10 "red")
                                                             (text (string-append (number->string(circ-vx (worldstate-circle2 w)))
                                                                                  ","
                                                                                  (number->string(circ-vy (worldstate-circle2 w)))) 10 "red"))
                                                            (list
                                                             (make-posn (reddot-x   (worldstate-reddot w)) (reddot-y   (worldstate-reddot w)))
                                                             (make-posn (circ-x (worldstate-circle1 w)) (circ-y (worldstate-circle1 w)))
                                                             (make-posn (circ-x (worldstate-circle2 w)) (circ-y (worldstate-circle2 w)))
                                                             (make-posn (circ-x (worldstate-circle1 w))(circ-y (worldstate-circle1 w)) )
                                                             (make-posn (circ-x (worldstate-circle2 w))(circ-y (worldstate-circle2 w)) ))
                                                            (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))]
    
    [else (place-images (list (circle 40 "outline" "blue")
                              (circle 40 "outline" "blue")
                              (text (string-append (number->string(circ-vx (worldstate-circle1 w)))
                                                   ","
                                                   (number->string(circ-vy (worldstate-circle1 w)))) 10 "red")
                              (text (string-append (number->string(circ-vx (worldstate-circle2 w)))
                                                   ","
                                                   (number->string(circ-vy (worldstate-circle2 w)))) 10 "red"))
                        (list (make-posn (circ-x (worldstate-circle1 w)) (circ-y (worldstate-circle1 w)))
                              (make-posn (circ-x (worldstate-circle2 w)) (circ-y (worldstate-circle2 w)))
                              (make-posn (circ-x (worldstate-circle1 w))(circ-y (worldstate-circle1 w)) )
                              (make-posn (circ-x (worldstate-circle2 w))(circ-y (worldstate-circle2 w)) ))
                        (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))]))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: use template for Worldstate on w
(define (world-after-mouse-event w mx my mev)
  (make-worldstate
    (circ-after-mouse-event (worldstate-circle1 w) mx my mev)
    (circ-after-mouse-event (worldstate-circle2 w) mx my mev)
    (worldstate-paused? w)
    (reddot-after-mouse-event (worldstate-reddot w) mx my mev))
    )



;; circl-after-mouse-event : circl Integer Integer MouseEvent -> circl
;; GIVEN: a circl and a description of a mouse event
;; RETURNS: the circl that should follow the given mouse event

;; strategy: use template for MousEevent mev
(define (circ-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "button-down")(circ-after-button-down c mx my)]
    [(mouse=? mev "drag") (circ-after-drag c mx my)]
    [(mouse=? mev "button-up") (circ-after-button-up c mx my)]
    [else c]))
;; reddot-after-mouse-event : reddot Integer Integer MouseEvent -> reddot
;; GIVEN: a reddot and a description of a mouse event
;; RETURNS: a reddot that should follow the given mouse event
;; strategy: use template for MousEevent mev
(define (reddot-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down")(reddot-after-button-down mx my)]
    [(mouse=? mev "button-up")(reddot-after-button-up mx my)]
    [(mouse=? mev "drag")(reddot-after-drag mx my)]
    [else r]
    ))
;; reddot-after-button-down : reddot Integer Integer -> reddot
;; GIVEN: a reddot and a description of a mouse
;; RETURNS: the reddot that should follow button-down
;; strategy: combine simpler functions
(define (reddot-after-button-down mx my)
  ( make-reddot mx my #t)
  )

  (define (reddot-after-button-up mx my)
  ( make-reddot mx my #f)
    )
(define (reddot-after-drag x y)
  (make-reddot x y #t)
     )
  

;; helper functions:

;; circl-after-button-down : circl Integer Integer -> circl
;; RETURNS: the circl following a button-down at the given location.
;; STRATEGY: Use template for circl on c
(define (circ-after-button-down c x y)
  (if (in-circl? c x y)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)  true (-(circ-x c) x) (-(circ-y c) y))
      c))

;; circl-after-drag : circl Integer Integer -> circl
;; GIVEN: circle mousex mousey
;; RETURNS: the circl following a drag at the given location
;; STRATEGY: Use template for circl on c
(define (circ-after-drag c x y)
  (if (circ-selected? c)
      (make-circ (+ x (circ-storedx c)) (+ y (circ-storedy c)) (circ-vx c) (circ-vy c) #t (circ-storedx c) (circ-storedy c))
      c))

;; circl-after-button-up: circl Integer Integer -> circl
;; GIVEN: circle mousex mousey
;; RETURNS: the circl following a button-up at the given location
;; STRATEGY: Use template for circl on c
;;EXAMPLES:
(define (circ-after-button-up c x y)
  ( if (circ-selected? c)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false 0 0)
      c))
  

;; in-circl?: circl Integer Integer -> Boolean
;; GIVEN: circle mousex mousey
;; RETURNS: true iff the given coordinate is inside the bounding box of
;; the given circle.
;; EXAMPLES: 
;; STRATEGY: Use template for circl on c
(define (in-circl? c x y)
  (<= (+ (sqr(- x (circ-x c)))
         (sqr(- y (circ-y c)))) (sqr 40) ))
