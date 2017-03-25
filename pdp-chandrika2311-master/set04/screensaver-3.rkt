;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; start with (main 0)

(require rackunit)
(require "extras.rkt")
(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         circle-after-key-event
         new-circle
         world-circles
         circ-x
         world-after-mouse-event
         circle-pen-down?
         world-paused?
         circ-after-mouse-event
         circ-selected?
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
;; specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver initial-pos1)
  (big-bang (initial-world initial-pos1)
            (on-tick world-after-tick 0.5 ) 
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                          CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Canvas dimensions can be constant 
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define RADIUS 40)
(define C-HITTING-FRONTY-AXIS 360)
(define C-HITTING-UPX-AXIS 360)
(define C-HITTING-BACKY-AXIS 360)
(define C-HITTING-LOWX-AXIS 260)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define STANDARD-CIRCLE (circle 10 "outline" "blue"))
(define RED-STANDARD-CIRCLE (circle 40 "outline" "red"))
(define BLUE-STANDARD-CIRCLE (circle 40 "outline" "blue"))
(define RED-DOT(circle 5 "solid" "red"))
(define DOT-CIRCLE (circle 1 "outline" "black"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                    TEMPLATE DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct dot(x y d?))
;;A circle is a
;;(make-dot Integer Integer
;;Interpretation:
;;x: x-coordinate of circle in pixels
;;y: y-coordinate of circle in pixels
;;d?: true iff dot is made
;;dot-fn: dot -> ??
;;(define (dot-fn c)
;;(... ((dot-x c)(dot-x c)


(define-struct circ (x y vx vy selected? lod dot? dx dy))
;;A circle is a
;;(make-circl Integer Integer Integer Integer lst Integer Integer)
;;Interpretation:
;;R :Radius
;;x: x-coordinate of circle in pixels
;;y: y-coordinate of circle
;;vx: velocity of circle in x direction
;;vy: velocity of circle in y direction
;;selected?: true if circle selected
;;lod :list of dots
;;dx: stores dx value
;;dy: stores dy value

;;circle-fn: circle -> ??
;;(define (circle-fn c)
;;(... ((circ-x c)(circ-x c)(circ-vx c)(circ-vy c)(circ-selected? c)(circ-lod c)(circ-dx c)(circ-dy c))
;; examples of circles, for testing:

(define selected-circle1-at-200-100 (make-circ 200 100 -12 20 true empty #f 0 0 ))
(define unselected-circle8-at-200-200 (make-circ 200 200 23 -14 false empty #f 0 0))
(define unselected-circle9-at-100-200 (make-circ 100 200 23 -14 false empty #f 0 0))
(define unselected-circle10-at-200-300 (make-circ 200 300 23 -14 false empty #f 0 0))

(define selected-circle2-at-200-100 (make-circ 200 100 -12 20 true empty #f 0 0 ))
(define selected-circle-at-200-100 (make-circ 200 100 -12 20 true empty #f 0 0 ))
(define selected-circlea-at-200-100 (make-circ 200 100 -12 18 true empty #f 0 0 ))
(define unselected-circle3-at-200-200 (make-circ 200 200 23 -14 false empty #f 0 0))

(define selected-circle5-at-200-100 (make-circ 10 100 -12 20 true empty #f 0 0))
(define unselected-circle6-at-200-200 (make-circ 20 20 23 -14 false empty #f 0 0))

(define set-of-circles1 (cons unselected-circle8-at-200-200(cons unselected-circle9-at-100-200
                                                                 (cons unselected-circle10-at-200-300 empty))))
(define set-of-circles2 (cons unselected-circle8-at-200-200(cons unselected-circle9-at-100-200
                                                                 (cons unselected-circle10-at-200-300(cons selected-circle2-at-200-100 empty)))))
(define set-of-circles3 (cons unselected-circle8-at-200-200(cons unselected-circle9-at-100-200
                                                                 (cons unselected-circle10-at-200-300(cons selected-circlea-at-200-100 empty)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct worldstate (loc paused? reddot clear?))
;; A WorldState is a (make-worldstate loc reddot)
;; Interpretation: 
;; loc: list of circles
;; reddot : A reddot
;; TEMPLATE
;; worldstate-fn : WorldState -> ??
;;(define (worldstate-fn w)
;;  (... (worldstate-loc w) (worldstate-paused? w)(worldstate-reddot w))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

;;List of Dots:
;------------------
A ListOfDots(LOD) is one of:
-empty
-(cons Dots LOD)
los-fn: LOD->??
;; (define (lod-fn lod)
;;   (cond
;;     [(empty? lod)...]
;;     [else (...
;;             (dot-fn (first lod))
;;             (loc-fn (rest lod)))]))

;; ListofCircles
-------------------
A ListOfCircles(LOC) is one of:
-- empty
-- (cons Circ LOC)

loc-fn : LOC -> ??
;; (define (loc-fn loc)
;;   (cond
;;     [(empty? loc)...]
;;     [else (...
;;             (circ-fn (first loc))
;;             (loc-fn (rest loc)))]))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct reddot(x y clicked?))
;; A RedDot is a (make-reddot x y Boolean)
;; Interpretation: 
;; x: is the x axis
;; y: is the y axis
;; clicked?: describes whether or not the mouse is clicked.
;; TEMPLATE
;; reddot-fn : Reddot -> ??

;;(define (reddot-fn r)
;;  (... (reddot-x r) (reddot-y r) (reddot-clicked? r))  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                  FUNCTIONS DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Functions related to world:
;;initial-world : Any -> WorldState
;; GIVEN:   Any value (ignored)
;; RETURNS: The initial world specified in the problem set
;;STRATEGY: Combining simpler functions
;;EXAMPLES:
;; (initial-world 1) =>(make-worldstate empty #f (reddot 0 0 #f) #f)
;; (initial-world 2) =>(make-worldstate empty #f (reddot 0 0 #f) #f)

(define (initial-world x)
  (make-worldstate  empty #t (make-reddot 0 0 #f) #f)
  )

;; TESTS
(begin-for-test
  (check-equal? (initial-world 1) (make-worldstate empty #t (make-reddot 0 0 #f) #f )
    "Empty world expected" )
  (check-equal? (initial-world 5) (make-worldstate empty #t (make-reddot 0 0 #f) #f )
    "Empty world expected"))



;____________________________________________________________________________________________________________________
;; is-u-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
;; STRATEGY: cases on kev : KeyEvent
(define (is-u-key-event? kev)
  (key=? kev "u"))
(begin-for-test
  (check-equal?(is-u-key-event? "u") #t))
;____________________________________________________________________________________________________________________
;; is-d-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
;; STRATEGY: cases on kev : KeyEvent
(define (is-d-key-event? kev)
  (key=? kev "d"))
(begin-for-test
  (check-equal?(is-d-key-event? "d") #t))
;____________________________________________________________________________________________________________________
;; is-e-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
;; STRATEGY: cases on kev : KeyEvent
(define (is-e-key-event? kev)
  (key=? kev "e"))
(begin-for-test
  (check-equal?(is-e-key-event? "e") #t))
;____________________________________________________________________________________________________________________
;; is-up-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a decrease in y velocity
;; STRATEGY: cases on kev : KeyEvent
(define (is-up-key-event? kev)
  (key=? kev "up"))
(begin-for-test
  (check-equal?(is-up-key-event? "up") #t))
;____________________________________________________________________________________________________________________
;; is-down-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a increase in y velocity
;; STRATEGY: cases on kev : KeyEvent
(define (is-down-key-event? kev)
  (key=? kev "down"))
(begin-for-test
  (check-equal?(is-down-key-event? "down") #t))
;____________________________________________________________________________________________________________________
;; is-left-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a decrease in x velocity
;; STRATEGY: cases on kev : KeyEvent
(define (is-left-key-event? kev)
  (key=? kev "left"))
(begin-for-test
  (check-equal?(is-left-key-event? "left") #t))
;____________________________________________________________________________________________________________________
;; is-right-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a increase in x velocity
;; STRATEGY: cases on kev : KeyEvent
(define (is-right-key-event? kev)
  (key=? kev "right"))
(begin-for-test
  (check-equal?(is-right-key-event? "right") #t))

;____________________________________________________________________________________________________________________
;; is-n-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a n instruction
;; STRATEGY: cases on kev : KeyEvent
(define (is-n-key-event? ke)
  (key=? ke "n"))
;;Tests:
(begin-for-test
  (check-equal?(is-n-key-event? "n") #t))

;____________________________________________________________________________________________________________________
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
;; STRATEGY: cases on kev : KeyEvent
(define (is-pause-key-event? ke)
  (key=? ke " "))
;;Tests:
(begin-for-test
  (check-equal?(is-pause-key-event? " ") #t))

;____________________________________________________________________________________________________________________
;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN:
;;       WorldState: current world state
;;       KeyEvent  : input of space bar
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; STRATEGY: cases on kev : KeyEvent


(define (world-after-key-event w kev) 
  (cond
  [(is-pause-key-event? kev) (world-with-paused-toggle w)]
  [(is-e-key-event? kev)     (make-worldstate (clear-list-circles-dots(worldstate-loc w)) (worldstate-paused? w) (worldstate-reddot w) (worldstate-clear? w))]
  [(is-u-key-event? kev)     (make-worldstate (pressed-u-circles(worldstate-loc w)) (worldstate-paused? w) (worldstate-reddot w) (worldstate-clear? w))]
  [(is-d-key-event? kev)     (make-worldstate (circles-with-dots(worldstate-loc w)) (worldstate-paused? w) (worldstate-reddot w) (worldstate-clear? w))]
  [(is-up-key-event? kev)    (make-worldstate (decrease-yvelocity (worldstate-loc w)) (worldstate-paused? w) (worldstate-reddot w) (worldstate-clear? w))]
  [(is-down-key-event? kev)  (make-worldstate (increase-yvelocity(worldstate-loc w)) (worldstate-paused? w) (worldstate-reddot w) (worldstate-clear? w))]
  [(is-left-key-event? kev)  (make-worldstate (decrease-xvelocity(worldstate-loc w)) (worldstate-paused? w) (worldstate-reddot w) (worldstate-clear? w))]
  [(is-right-key-event? kev) (make-worldstate (increase-xvelocity(worldstate-loc w)) (worldstate-paused? w) (worldstate-reddot w) (worldstate-clear? w))]
  [(is-n-key-event? kev)     (add-circle-to-centre w)]
   [else w] ))

(begin-for-test 
  (check-equal?
    (world-after-key-event (initial-world 3) "n" )
    
    (make-worldstate (list (make-circ 200 150 0 0 #false '() #false 0 0)) #true (make-reddot 0 0 #false) #false)
    "After n key event a new circle at centre did not get created")
(check-equal?
    (world-after-key-event
     (make-worldstate (list (make-circ 200 150 0 0 #t '() #false 1 1)) #true (make-reddot 209 173 #false) #false) "up" )
    (make-worldstate (list (make-circ 200 150 0 -2 #t '() #false 1 1)) #t (make-reddot 209 173 #f) #f)
    "After n key event a new circle at centre did not get created")
(check-equal?
    (world-after-key-event
     (make-worldstate (list (make-circ 200 150 0 0 #t '() #false 1 1)) #true (make-reddot 209 173 #false) #false) "down" )
    (make-worldstate (list (make-circ 200 150 0 2 #t '() #false 1 1)) #t (make-reddot 209 173 #f) #f)
    "After n key event a new circle at centre did not get created")
(check-equal?
    (world-after-key-event
     (make-worldstate (list (make-circ 200 150 0 0 #t '() #false 1 1)) #true (make-reddot 209 173 #false) #false) "left" )
    (make-worldstate (list (make-circ 200 150 -2 0 #t '() #false 1 1)) #t (make-reddot 209 173 #f) #f)
    "After n key event a new circle at centre did not get created")
(check-equal?
    (world-after-key-event
     (make-worldstate (list (make-circ 200 150 0 0 #t '() #false 1 1)) #true (make-reddot 209 173 #false) #false) "right" )
    (make-worldstate (list (make-circ 200 150 2 0 #t '() #false 1 1)) #t (make-reddot 209 173 #f) #f)
    "After n key event a new circle at centre did not get created")
(check-equal?
    (world-after-key-event
     (make-worldstate (list (make-circ 200 150 0 0 #t '() #f 1 1)) #f (make-reddot 209 173 #false) #false) " " )
    (make-worldstate (list (make-circ 200 150 0 0 #t '() #f 1 1)) #t (make-reddot 209 173 #f) #f)
    "After n key event a new circle at centre did not get created")
(check-equal?
    (world-after-key-event
     (make-worldstate (list (make-circ 200 150 0 0 #t '() #f 1 1)) #f (make-reddot 209 173 #false) #false) "c" )
    (make-worldstate (list (make-circ 200 150 0 0 #t '() #f 1 1)) #f (make-reddot 209 173 #false) #false)
    "After n key event a new circle at centre did not get created")
(check-equal?
    (world-after-key-event
    (make-worldstate (list (make-circ 200 150 2 10 #t '() #f 1 1)(make-circ 210 150 34 -12 #f '() #f 1 1)) #f (make-reddot 209 173 #false) #false) "d" )
    (make-worldstate (list (make-circ 200 150 2 10 #t (list(make-dot 200 150 #t)) #t 1 1)(make-circ 210 150 34 -12 #f '() #f 1 1)) #f (make-reddot 209 173 #false) #false)
    "After n key event a new circle at centre did not get created")
(check-equal?
    (world-after-key-event
    (make-worldstate (list (make-circ 200 150 2 10 #t (list(make-dot 200 150 #t)) #t 1 1)(make-circ 210 150 34 -12 #f '() #f 1 1)) #f (make-reddot 209 173 #false) #false) "u" )
    (make-worldstate (list (make-circ 200 150 2 10 #t (list(make-dot 200 150 #t)) #f 1 1)(make-circ 210 150 34 -12 #f '() #f 1 1)) #f (make-reddot 209 173 #false) #false)
    "After n key event a new circle at centre did not get created")
(check-equal?
    (world-after-key-event
    (make-worldstate (list (make-circ 200 150 2 10 #t (list(make-dot 200 150 #t)) #t 1 1)(make-circ 210 150 34 -12 #f '() #f 1 1)) #f (make-reddot 209 173 #false) #false) "e" )
    (make-worldstate (list (make-circ 200 150 2 10 #t '() #f 1 1)(make-circ 210 150 34 -12 #f '() #f 1 1)) #f (make-reddot 209 173 #false) #false)
    "After n key event a new circle at centre did not get created")


  )
;____________________________________________________________________________________________________________________

;; circle-after-key-event : Circle KeyEvent -> Circle
;;GIVEN: Circle
;; RETURNS: the state of the circle that should follow the given
;; circle after the given key event
;; help function for key event
(define (circle-after-key-event c kev)
  (cond
  [(is-pause-key-event? kev) c]
  [(is-e-key-event? kev)     (clear-circle-dots c)]
  [(is-u-key-event? kev)     (pressed-u-1circle c)]
  [(is-d-key-event? kev)     (make-circ(circ-x c)(circ-y c)(circ-vx c) (circ-vy c) (circ-selected? c)
                                       (list-of-dot(circ-x c) (circ-y c) empty) #t (circ-dx c) (circ-dy c))]
  [(is-up-key-event? kev)    (decrease-circ-yvelocity c)]
  [(is-down-key-event? kev)  (increase-circ-yvelocity c)]
  [(is-left-key-event? kev)  (decrease-circ-xvelocity c)]
  [(is-right-key-event? kev) (increase-circ-xvelocity c)]
  [(is-n-key-event? kev)     c]
   [else c] ))

;;-----------------------Tests
(begin-for-test (check-equal? (circle-after-key-event(make-circ 200 150 0 0 #t empty #false 1 1) " ")(make-circ 200 150 0 0 #t '() #false 1 1))
                (check-equal? (circle-after-key-event(make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1) "e")
                              (make-circ 200 150 0 0 #t empty #false 1 1))
                (check-equal? (circle-after-key-event(make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1) "u")
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1))
                (check-equal? (circle-after-key-event
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1) "d")
                              (make-circ 200 150 0 0 #t (cons(make-dot 200 150 #t) empty) #t 1 1))
                (check-equal? (circle-after-key-event
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1) "up")
                              (make-circ 200 150 0 -2 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #f 1 1))
                (check-equal? (circle-after-key-event
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1) "down")
                              (make-circ 200 150 0 2 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #f 1 1))
                (check-equal? (circle-after-key-event
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1) "left")
                              (make-circ 200 150 -2 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #f 1 1))
                (check-equal? (circle-after-key-event
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1) "right")
                              (make-circ 200 150 2 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #f 1 1))
                (check-equal? (circle-after-key-event
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1) "n")
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1))
                (check-equal? (circle-after-key-event
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1) "j")
                              (make-circ 200 150 0 0 #t (list(make-dot 3 3 #t)(make-dot 4 3 #t)) #false 1 1)))
;____________________________________________________________________________________________________________________
;; circles-with-dots : Circles -> Circles
;; GIVEN:List of Circles
;; RETURNS: list of dots
;;STRATEGY: Use template for LOS on lst
;;EXAMPLES:(circles-with-dots(list (make-circ 200 150 2 10 #t (list(make-dot 200 150 #t)) #t 1 1)
;;          (make-circ 210 150 34 -12 #f '() #f 1 1)))=>
;;          (list (make-circ 200 150 2 10 #t (list(make-dot 200 150 #t)) #t 1 1)
;;          (make-circ 210 150 34 -12 #f '() #f 1 1))

(define (circles-with-dots circles)
  (cond
    [(empty? circles) empty]
    [else (cons (circle-draw-dot (first circles))(circles-with-dots (rest circles)))]))
;;-----------------------Tests
(begin-for-test(check-equal?
    (world-after-key-event
     (make-worldstate (circles-with-dots(list (make-circ 200 150 2 10 #t '() #f 1 1)
                                              (make-circ 210 150 34 -12 #f '() #f 1 1))) #f
                      (make-reddot 209 173 #false) #false)"d")
     (make-worldstate (list (make-circ 200 150 2 10 #t (list(make-dot 200 150 #t)) #t 1 1)
                                              (make-circ 210 150 34 -12 #f '() #f 1 1)) #f
                      (make-reddot 209 173 #false) #false)
    
    "After n key event a new circle at centre did not get created"))
;____________________________________________________________________________________________________________________
;; circles-draw-dot : Circles -> Circles
;; GIVEN:List of Circles
;; RETURNS: list of dots
;; EXAMPLES:(circle-draw-dot (make-circ 210 150 34 -12 #f '(list(make-dot(210 150 #t)) #f 1 1))
;; STRATEGY: Use template for LOS on lst
(define (circle-draw-dot c)
  (cond
    [(circ-selected? c) (make-circ(circ-x c)(circ-y c)(circ-vx c) (circ-vy c) (circ-selected? c)
                                  (list-of-dot(circ-x c) (circ-y c) empty) #t (circ-dx c) (circ-dy c))]
     [else c]))
;;-----------------------Tests
(begin-for-test (check-equal? (circle-draw-dot(make-circ 210 150 34 -12 #t '() #f 1 1)) (make-circ 210 150 34 -12 #t(list(make-dot 210 150 #t))#t 1 1)))
;____________________________________________________________________________________________________________________
;; list-of-dot : x y dots -> Dots
;; GIVEN:x y dots
;; RETURNS: appends a dot into a list of dots
;; EXAMPLES:(list-of-dot 200 300 (list(make-dot 2 3 #t)))=>(list(make-dot 200 300 #t)(make-dot 2 3 #t))
;; STRATEGY: Use template for LOS on lst

(define (list-of-dot x y dots)
  (cons(make-dot x y #t) dots))
(begin-for-test(check-equal?(list-of-dot 200 300 (list(make-dot 2 3 #t))) (list(make-dot 200 300 #t)(make-dot 2 3 #t))))

;;**************************;;**************************;;**************************;;**************************

;;pressed-u-circles loc -> loc
;;GIVEN : list of circles
;RETURNS : List Of Circles
(define (pressed-u-1circle c)
  (cond
    [(circ-selected? c) (make-circ(circ-x c)(circ-y c)(circ-vx c) (circ-vy c) (circ-selected? c)
                                  (circ-lod c) #f (circ-dx c) (circ-dy c))]
     [else c]))

(define (pressed-u-circles circles)
  (cond
    [(empty? circles) empty]
    [else (cons (pressed-u-1circle (first circles))(pressed-u-circles (rest circles)))]))

;;**************************
(define (clear-list-circles-dots circles)
  (cond
    [(empty? circles) empty]
    [else (cons (clear-circle-dots (first circles))(clear-list-circles-dots (rest circles)))]))
;;clear-circle-dots : w -> w
;;GIVEN: worldstate
;;RETURNS: worldstate
;EXAMPLES:
;;(clear-circle-dots (make-worldstate empty (make-reddot 0 0) #f)) => (make-worldstate empty (make-reddot 0 0) #t )"use in draw function
;;(clear-circle-dots (make-worldstate empty (make-reddot 0 0) #t)) => (make-worldstate empty (make-reddot 0 0) #t )"use in draw function
;;STRATEGY: cases on template w on clear?
(define (clear-circle-dots c)
  (cond
    ;[(circ-selected? c) (make-circ(circ-x c)(circ-y c)(circ-vx c) (circ-vy c) (circ-selected? c) empty #f (circ-dx c) (circ-dy c))]
    [(circ-selected? c) (make-circ(circ-x c)(circ-y c)(circ-vx c) (circ-vy c) (circ-selected? c) empty #f (circ-dx c) (circ-dy c))]
     [else c]) )
;;-----------------------Tests
;;Tested in circle-at-key-event
  
;____________________________________________________________________________________________________________________
;; decrease-yvelocity : Circles -> Circles
;; GIVEN: List of Circles
;; RETURNS: List Of Circles with new velocity
;;Examples: (decrease-yvelocity set-of-circles2)=>set-of-circles3
;;Strategy:
(define(decrease-yvelocity circles)
  (cond
    [(empty? circles) empty]
    [else (cons(decrease-circ-yvelocity (first circles )) (decrease-yvelocity(rest circles )))]
))
(begin-for-test(check-equal? (decrease-yvelocity set-of-circles2)set-of-circles3))
;____________________________________________________________________________________________________________________
;; increase-yvelocity : Circles -> Circles
;; GIVEN: List of Circles
;; RETURNS: List Of Circles with new velocity
;;Examples:(increase-yvelocity set-of-c) => set-of-c1
;;Strategy:Use template for LOS on lst
(define(increase-yvelocity circles)
  (cond
    [(empty? circles) empty]
    [else (cons(increase-circ-yvelocity (first circles )) (increase-yvelocity(rest circles )))]
))

;;-----------------------Tests

(define selected-c1-at-200-100 (make-circ 200 100 -12 20 true empty #f 0 0 ))
(define selected-c11-at-200-100 (make-circ 200 100 -12 22 true empty #f 0 0 ))

(define set-of-c (cons unselected-circle8-at-200-200(cons unselected-circle9-at-100-200
                                                     (cons unselected-circle10-at-200-300
                                                     (cons selected-c1-at-200-100 empty)))))
(define set-of-c1 (cons unselected-circle8-at-200-200(cons unselected-circle9-at-100-200
                                                     (cons unselected-circle10-at-200-300
                                                     (cons selected-c11-at-200-100 empty)))))

(begin-for-test(check-equal? (increase-yvelocity set-of-c)set-of-c1))

;____________________________________________________________________________________________________________________
;; decrease-xvelocity : Circles -> Circles
;; GIVEN: List of Circles
;; RETURNS: List Of Circles with new velocity
;;Examples:(decrease-xvelocity set-of-c) => set-of-c1
;;Strategy:Use template for LOS on lst

(define(decrease-xvelocity circles)
  (cond
    [(empty? circles) empty]
    [else (cons(decrease-circ-xvelocity (first circles )) (decrease-xvelocity(rest circles )))]
))
;;-----------------------Tests
(define selected-c111-at-200-100 (make-circ 200 100 -14 20 true empty #f 0 0 ))

(define set-of-c11 (cons unselected-circle8-at-200-200(cons unselected-circle9-at-100-200
                                                     (cons unselected-circle10-at-200-300
                                                     (cons selected-c111-at-200-100 empty)))))
(begin-for-test(check-equal? (decrease-xvelocity set-of-c)set-of-c11))
;____________________________________________________________________________________________________________________;;**************************
;; increase-xvelocity : Circles -> Circles
;; GIVEN: List of Circles
;; RETURNS: List Of Circles with new velocity
;;Examples:(increase-xvelocity set-of-c) => set-of-c1
;;Strategy:Use template for LOS on lst
(define(increase-xvelocity circles)
  (cond
    [(empty? circles) empty]
    [else (cons(increase-circ-xvelocity (first circles )) (increase-xvelocity(rest circles )))]))
;;-----------------------Tests
(define selected-c12-at-200-100 (make-circ 200 100 -10 20 true empty #f 0 0 ))
(define set-of-c12 (cons unselected-circle8-at-200-200(cons unselected-circle9-at-100-200
                                                     (cons unselected-circle10-at-200-300
                                                     (cons selected-c12-at-200-100 empty)))))
(begin-for-test(check-equal? (increase-xvelocity set-of-c)set-of-c12))
;____________________________________________________________________________________________________________________
;;decrease-circ-yvelocity: circ -> circ
;;GIVEN:Circle 
;;RETHRNS: circle with new velocity
;;STRATEGY: Use template for Circ on selected?
;;EXAMPLES:(decrease-circ-yvelocity (make-circ 200 200 0 0 #t empty #f 0 0 ))=> (make-circ 200 200 0 -2 #t empty #f 0 0)
(define (decrease-circ-yvelocity c)
  ( cond[(circ-selected? c) (make-circ(circ-x c) (circ-y c) (circ-vx c) (- (circ-vy c) 2) (circ-selected? c)(circ-lod c)(circ-dot? c) (circ-dx c)(circ-dy c))]
        [else c]))
;;-----------------------Tests
(begin-for-test
  (check-equal?(decrease-circ-yvelocity (make-circ 200 200 0 0 #t empty #f 0 0 ))(make-circ 200 200 0 -2 #t empty #f 0 0))
  (check-equal?(decrease-circ-yvelocity (make-circ 200 200 0 0 #f empty #f 0 0 ))(make-circ 200 200 0 0 #f empty #f 0 0)))
;____________________________________________________________________________________________________________________
;;increase-circ-yvelocity: circ -> circ
;;GIVEN:Circle 
;;RETURNS: circle with new velocity
;;STRATEGY: Use template for Circ on selected?
;;EXAMPLES:(increase-circ-yvelocity (make-circ 200 200 0 0 #t empty #f 0 0 ))=> (make-circ 200 200 0 2 #t empty #f 0 0)
(define (increase-circ-yvelocity c)
  ( cond[(circ-selected? c) (make-circ(circ-x c) (circ-y c) (circ-vx c) (+ (circ-vy c) 2)(circ-selected? c)(circ-lod c)(circ-dot? c)(circ-dx c)(circ-dy c))]
        [else c]))
;;-----------------------Tests
(begin-for-test
  (check-equal?(increase-circ-yvelocity (make-circ 200 200 0 0 #t empty #f 0 0 ))(make-circ 200 200 0 2 #t empty #f 0 0))
  (check-equal?(increase-circ-yvelocity (make-circ 200 200 0 0 #f empty #f 0 0 ))(make-circ 200 200 0 0 #f empty #f 0 0)))

;____________________________________________________________________________________________________________________
;;decrease-circ-xvelocity: circ -> circ
;;GIVEN:Circle 
;;RETURNS: circle with new velocity
;;STRATEGY: Use template for Circ on selected?
;;EXAMPLES:(decrease-circ-xvelocity (make-circ 200 200 0 0 #t empty #f 0 0 ))=> (make-circ 200 200 -2 0 #t empty #f 0 0)
(define (decrease-circ-xvelocity c)
  ( cond[(circ-selected? c) (make-circ(circ-x c) (circ-y c) (- (circ-vx c) 2) (circ-vy c)(circ-selected? c)(circ-lod c)(circ-dot? c)(circ-dx c)(circ-dy c))]
        [else c]))
;;-----------------------Tests
(begin-for-test
  (check-equal?(decrease-circ-xvelocity (make-circ 200 200 0 0 #t empty #f 0 0 ))(make-circ 200 200 -2 0 #t empty #f 0 0))
  (check-equal?(decrease-circ-xvelocity (make-circ 200 200 0 0 #f empty #f 0 0 ))(make-circ 200 200 0 0 #f empty #f 0 0)))
;____________________________________________________________________________________________________________________
;;increase-circ-xvelocity: circ -> circ
;;GIVEN:Circle 
;;RETURNS: circle with new velocity
;;STRATEGY: Use template for Circ on selected?
;;EXAMPLES:(increase-circ-xvelocity (make-circ 200 200 0 0 #t empty #f 0 0 ))=> (make-circ 200 200 2 0 #t empty #f 0 0)
(define (increase-circ-xvelocity c)
  ( cond[(circ-selected? c) (make-circ(circ-x c) (circ-y c) (+ (circ-vx c) 2) (circ-vy c)(circ-selected? c)(circ-lod c)(circ-dot? c)(circ-dx c)(circ-dy c))]
        [else c]))
;;-----------------------Tests
(begin-for-test
  (check-equal?(increase-circ-xvelocity (make-circ 200 200 0 0 #t empty #f 0 0 ))(make-circ 200 200 2 0 #t empty #f 0 0))
  (check-equal?(increase-circ-xvelocity (make-circ 200 200 0 0 #f empty #f 0 0 ))(make-circ 200 200 0 0 #f empty #f 0 0)))

;____________________________________________________________________________________________________________________
;;world-with-paused-toggle: WorldState -> WorldState
;;GIVEN: WorldState
;;RETURNS: a world like the given one, but with paused? toggled
;;STRATEGY: Use template for world on w

(define (world-with-paused-toggle w)
  ( make-worldstate(worldstate-loc w) (not (worldstate-paused? w)) (worldstate-reddot w) (worldstate-clear? w))
   )
;;-----------------------Tests
(begin-for-test
  (check-equal?(world-with-paused-toggle(make-worldstate (cons(new-circle 200 100 -12 20) empty) #f (make-reddot 0 0 #f) #f ))
               (make-worldstate (cons(new-circle 200 100 -12 20) empty) #t (make-reddot 0 0 #f) #f)))

;____________________________________________________________________________________________________________________

;;add-circle-to-centre: w -> w
;given : worldstate
;returns: worldstate with a new circle at centre
;example:
;(add-circle-to-centre (make-worldstate empty (make-reddot 0 0 #f) #f )) =>(make-worldstate (cons (circle-at-centre 400 300)) (make-reddot 0 0 #f) #f )

(define(add-circle-to-centre w)
  (make-worldstate (cons(circle-at-centre 400 300)(worldstate-loc w)) (worldstate-paused? w) (worldstate-reddot w) #f))

;;-----------------------Tests
(begin-for-test
  (check-equal? (add-circle-to-centre (make-worldstate empty #f (make-reddot 0 0 #f) #f ))
                (make-worldstate (cons (circle-at-centre 400 300) empty) #f (make-reddot 0 0 #f) #f )))
;____________________________________________________________________________________________________________________

;;circle1  x y -> circle
;;GIVEN: X AND Y
;;RETURNS: circle
;;example: self evident
;;STRATEGY: combine simpler functions

(define(circle1 x y)
  (make-circ x y -12 30 #f empty #f 0 0))

(begin-for-test (check-equal?(circle1 200 300)(make-circ 200 300 -12 30 #f empty #f 0 0)))


;____________________________________________________________________________________________________________________

;;circle-at-centre: any ->circle
;;GIVEN: CANVAS-WIDTH CANVAS-HEIGHT
;;RETURNS: circle
;;example:
;;(circle-at-centre 2)=> (make-circ(/ 2 CANVAS-WIDTH )(/ 2 CANVAS-HEIGHT) 0 0 #f 0 0 )
(define (circle-at-centre CANVAS-WIDTH CANVAS-HEIGHT)
  (make-circ(/ CANVAS-WIDTH 2)(/ CANVAS-HEIGHT 2) 0 0 #f empty #f 0 0 ))
;;-----------------------Tests
(begin-for-test
  (check-equal?(circle-at-centre 400 400) (make-circ 200 200 0 0 #f empty #f 0 0 ) "A circle at the centre should be returned"))

;____________________________________________________________________________________________________________________
;;scene-selected-circle?: circle -> boolean
;;given: selected circle
;;returns: circle
;; examples: (scene-selected-circle (make-circ 200 200 0 0 #t empty 0 0 ))=> #t
;; examples: (scene-selected-circle (make-circ 200 200 0 0 #f  empty 0 0 ))=> #f
(define (scene-selected-circle? c)
  (circ-selected? c))
;;-----------------------Tests
(begin-for-test (check-equal? (scene-selected-circle? (make-circ 200 200 0 0 #t empty #f 0 0 )) #t))

;____________________________________________________________________________________________________________________
;;scene-unselt: circle scene -> scene
;;GIVEN: circle and current scene
;;RETURN: scene
;;examples:(scene-unselt (make-circ 200 200 0 0 #f empty 0 0 )) =>
;;                              (place-images (list BLUE-STANDARD-CIRCLE
;                                   DOT-CIRCLE
;                                   (text (string-append (number->string(circ-vx c))","(number->string(circ-vy c))) 10 "red"))
;                             (list (make-posn 200 200)
;                                   (make-posn 200 200)
;                                   (make-posn 200 200))
;                             (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define (scene-unselt c s)
             (place-images (list BLUE-STANDARD-CIRCLE
                                 (text (string-append (number->string(circ-vx c))","(number->string(circ-vy c))) 10 "red"))
                           (list (make-posn (circ-x c) (circ-y c))
                                 
                                 (make-posn (circ-x c) (circ-y c)))
                           s))
;____________________Tests
(begin-for-test(check-equal?(scene-unselt (make-circ 200 200 10 10 #f empty #f 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
                            (place-images (list BLUE-STANDARD-CIRCLE
                                 (text (string-append (number->string 10)","(number->string 10)) 10 "red"))
                           (list (make-posn 200 200)
                                 (make-posn 200 200))
                           (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))))
;____________________________________________________________________________________________________________________
;;scene-sel: circle scene -> scene
;;GIVEN: circle and current scene
;;RETURN: scene with images on it
;;STRATEGY: Image placements on screen
;;examples:(scene-sel (make-circ 200 200 0 0 #f #f 0 0 )) =>
;;                              (place-images (list RED-STANDARD-CIRCLE
;                                   DOT-CIRCLE
;                                   (text (string-append (number->string(circ-vx c))","(number->string(circ-vy c))) 10 "red"))
;                             (list (make-posn 200 200)
;                                   (make-posn 200 200)
;                                   (make-posn 200 200))
;                             (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define (scene-sel c s)
  (place-images (list RED-STANDARD-CIRCLE
                                   (text (string-append (number->string(circ-vx c))","(number->string(circ-vy c))) 10 "red"))
                             (list(make-posn (circ-x c) (circ-y c))
                                   (make-posn (circ-x c) (circ-y c)))
                             s))

(begin-for-test(check-equal?(scene-sel (make-circ 200 200 10 10 #f empty #f 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
                            (place-images (list RED-STANDARD-CIRCLE
                                 (text (string-append (number->string 10)","(number->string 10)) 10 "red"))
                           (list (make-posn 200 200)
                                 (make-posn 200 200))
                           (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))))

;____________________________________________________________________________________________________________________
;;(place-images-on-canvas STANDARD-CIRCLE point? cx cy px py)
;;GIVEN: Circle Scene
;;RETURNS: scene with circle and point
(define(place-images-on-canvas c s)
(cond
     [(and (circ-selected? c)(circ-dot? c)) (scene-sel c (list-dot (circ-lod c) s))]
     [(and (circ-selected? c)(not(circ-dot? c))) (scene-sel c (list-dot (circ-lod c) s))]
     [(circ-dot? c) (scene-unselt c (list-dot (circ-lod c) s))]
     [else (scene-unselt c (list-dot (circ-lod c) s))]
     ))
;____________________Tests
(begin-for-test(check-equal?(place-images-on-canvas (make-circ 200 200 10 10 #t empty #t 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
                            (scene-sel (make-circ 200 200 10 10 #f empty #f 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))
               (check-equal?(place-images-on-canvas (make-circ 200 200 10 10 #t empty #f 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
                            (scene-sel (make-circ 200 200 10 10 #f empty #f 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))
               (check-equal?(place-images-on-canvas (make-circ 200 200 10 10 #f empty #t 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
                            (scene-unselt (make-circ 200 200 10 10 #f empty #f 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))
               (check-equal?(place-images-on-canvas (make-circ 200 200 10 10 #f empty #f 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
                            (scene-unselt (make-circ 200 200 10 10 #f empty #f 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))))
;____________________________________________________________________________________________________________________
;;blank-scene: string -> scene
;;GIVEN : string
;;RETURNS: scene with blank canvas
;;Example:blank-scene("empty") -> (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)
;;STRATEGY: combining simpler functions
  (define (blank-scene s)
   (cond[ (string=? s  "empty")
        (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)]
        ))
(begin-for-test (check-equal?(place-images-on-canvas (make-circ 200 200 10 10 #f empty #f 0 0 ) (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
                            (scene-unselt (make-circ 200 200 10 10 #f empty #f 0 0 ) (blank-scene "empty")))) 
;____________________________________________________________________________________________________________________  
;;draw-list-content: circles -> scene
;;GIVEN:ListOfcircles called circles
;;RETURNS: Scene
;;EXAMPLE:See test cases
;; STRATEGY: Use template for LOC on circles
(define (draw-list-content circles)
  (cond
    [(empty? circles) (blank-scene "empty")]
    [else (place-images-on-canvas (first circles) (draw-list-content(rest circles))) ]))

(begin-for-test(check-equal?(draw-list-content(list (make-circ 200 200 10 10 #t empty #t 0 0 )) )
                            (place-images-on-canvas (make-circ 200 200 10 10 #t empty #f 0 0 )(blank-scene "empty"))))

;____________________________________________________________________________________________________________________  
;;GIVEN: Dot and a scene
;;RETURN : one dot on the scene is returned
;;EXAMPLE:See test cases
;;STRATEGY: Combine simpler functions
;;one dot scene returned
(define (place-dot-on-canvas d s)
 (place-image DOT-CIRCLE
              (dot-x d)
              (dot-y d)
              s ))

(begin-for-test (check-equal? (place-dot-on-canvas (make-dot 2 3 #t) (blank-scene "empty"))(place-image DOT-CIRCLE 2 3 (blank-scene "empty"))))
;____________________________________________________________________________________________________________________  

;GIVEN:list-dot dots scene-> list of dots r
;;RETURNED scene
;;STRATEGY:Use template for LOC
;EXAMPLES: see test case

(define (list-dot dots s)
  (cond
    [(empty? dots) s]
    [else (place-dot-on-canvas(first dots)(list-dot(rest dots) s))]))
(begin-for-test(check-equal?(list-dot (list(make-dot 2 3 #t))(blank-scene "empty")) (place-dot-on-canvas (make-dot 2 3 #t) (blank-scene "empty"))))
;____________________________________________________________________________________________________________________  
;GIVEN:create-reddot: dots scene-> list of dots r
;;RETURNED scene
;;STRATEGY:Use template for LOC
;EXAMPLES: see test case

(define (create-reddot r s)
  (place-image
    RED-DOT
    (reddot-x r)
    (reddot-y r)
    s))

(begin-for-test(check-equal?(create-reddot (make-reddot 2 3 #t) (blank-scene "empty"))(place-image
    RED-DOT
    2
    3
    (blank-scene "empty"))))
;____________________________________________________________________________________________________________________  


;; world-to-scene : World -> Scene
;; GIVEN: WorldStaten
;; RETURNS: a Scene that portrays the given worldstate.
;; STRATEGY: Use template for worldstate on w

(define (world-to-scene w)
  (cond
    [(reddot-clicked? (worldstate-reddot w)) (create-reddot (worldstate-reddot w) (draw-list-content (worldstate-loc w)))]
    ;[(worldstate-clear? w)  (draw-list-content (worldstate-loc w))]
    [else (draw-list-content (worldstate-loc w))]
    
))


;____________________________________________________________________________________________________________________  
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

;____________________________________________________________________________________________________________________


;(define C-HITTING-BACKY-AXIS 360)
;(define C-HITTING-LOWX-AXIS 260)

;;x-left-side?: x -> Boolean
;;GIVEN: value of x Integer
;;RETURNS: Boolean after checking if circle hitting y axis or not, returns true if circle touching boundary on front y axis only 
;;EXAMPLES:
;;(c-left-side? 39) => #t
;;(c-left-side? 361) => #t
;;STRATEGY: Combine simpler functions
(define(x-left-side? x)
  (<= x RADIUS))

(begin-for-test
  (check-equal? (x-left-side? 39) #t "True expected")
  (check-equal? (x-left-side? 40) #t "True expected")
  (check-equal? (x-left-side? 361) #f "False expected"))
;____________________________________________________________________________________________________________________                  
;;x-right-side?: x -> Boolean
;;GIVEN: value of x Integer
;;RETURNS: Boolean after checking if circle hitting y axis or not, returns true if circle touching boundary on back y axis only 
;;EXAMPLES:
;;(x-right-side? 359) => #f
;;(x-right-side? 361) => #t
;;STRATEGY: Combine simpler functions
(define(x-right-side? x)
  (>= x C-HITTING-BACKY-AXIS))
(begin-for-test
  (check-equal? (x-right-side? 359) #f "False expected")
  (check-equal? (x-right-side? 360) #t "True expected")
  (check-equal? (x-right-side? 361) #t "True expected"))
;____________________________________________________________________________________________________________________  
;;y-up?: y -> Boolean
;;GIVEN: value of y Integer
;;RETURNS: Boolean after checking if circle hitting x axis or not
;;EXAMPLES:
;;(y-up? 39) => #t
;;((y-up? 42) => #f
;;STRATEGY: Combine simpler functions
(define(y-up? y)
  ( <= y RADIUS)
   )
(begin-for-test
  (check-equal? (y-up? 39) #t "True expected")
  (check-equal? (y-up? 42) #f "False expected"))
;____________________________________________________________________________________________________________________  
;;y-down?: y -> Boolean
;;GIVEN: value of y Integer
;;RETURNS: Boolean after checking if circle hitting x axis or not
;;EXAMPLES:
;;(y-down? 261) => #t
;;((y-down? 259) => #f
;;STRATEGY: Combine simpler functions
(define (y-down? y)
  (  >= y C-HITTING-LOWX-AXIS)
   )
(begin-for-test
  (check-equal? (y-down? 261) #t "True expected")
  (check-equal? (y-down? 259) #f "False expected"))

;____________________________________________________________________________________________________________________  
;;Calc-newx : c -> Integer
;;GIVEN: Circle
;;RETURNS: Value of new x after tick
;;STRATEGY: cases on x 
;;EXAMPLES:
;;(calc-newx (new-circle 200 100 10 -20)) => 210
;;(calc-newx (new-circle 40 100 10 -20)) => 30
;;(calc-newx (new-circle 360 100 10 -20)) => 350


(define (calc-newx c)
  ( cond
     [(x-in-middle? (circ-x c)) (circle_new_position (circ-x c)(circ-vx c))]
     [(x-right-side? (circ-x c)) (circle_new_position C-HITTING-BACKY-AXIS (* -1 (circ-vx c)))]
     [(x-left-side? (circ-x c)) (circle_new_position RADIUS (* -1 (circ-vx c))) ]

   ))
(begin-for-test
  (check-equal? (calc-newx (make-circ 200 100 10 -20 #f empty #f 0 0)) 210)
  (check-equal? (calc-newx (make-circ 200 100 10 -20 #f empty #f 0 0)) 210)
  (check-equal? (calc-newx (make-circ 39 100 -10 -20 #f empty #f 0 0)) 50)
  (check-equal? (calc-newx (make-circ 361 100 10 20 #f empty #f 0 0)) 350))

;____________________________________________________________________________________________________________________  
;;Calc-newy : c -> Integer
;;GIVEN: Circle
;;RETURNS: Value of new y after tick
;;STRATEGY: cases on y 
;;EXAMPLES:
;;(calc-newy (new-circle 200 100 10 -20)) => 210
;;(calc-newy (new-circle 40 100 10 -20)) => 30
;;(calc-newy (new-circle 360 100 10 -20)) => 350
(define (calc-newy c)
  ( cond
     [(y-in-middle? (circ-y c)) (circle_new_position (circ-y c)(circ-vy c))]
     [(y-up? (circ-y c)) (circle_new_position RADIUS ( * -1 (circ-vy c)))]
     [(y-down? (circ-y c)) (circle_new_position C-HITTING-LOWX-AXIS ( * -1 (circ-vy c)))])  )
(begin-for-test
  (check-equal? (calc-newy (make-circ 100 100 10 -20 #f empty #f 0 0)) 80)
  (check-equal? (calc-newy (make-circ 50 39 -10 -20 #f empty #f 0 0)) 60)
  (check-equal? (calc-newy (make-circ 60 261 -10 20 #f empty #f 0 0)) 240))
;____________________________________________________________________________________________________________________  

;;Calc-newvx : c -> Integer
;;GIVEN: Circle
;;RETURNS: Value of new vx after tick
;;STRATEGY: cases on vx 
;;EXAMPLES:
;;(calc-newvx (new-circle 200 100 10 -20)) => 10
;;(calc-newvx(new-circle 40 100 10 -20)) => -10
;;(calc-newvx (new-circle 360 100 10 -20)) => -10
  
  (define (calc-newvx c)
   (cond
      [(x-in-middle? (circ-x c)) (circ-vx c)]  
      [else (* -1 (circ-vx c))]))
(begin-for-test
  (check-equal? (calc-newvx (make-circ 200 100 10 -20 #f empty #f 0 0)) 10)
  (check-equal? (calc-newvx (make-circ 362 100 10 -20 #f empty #f 0 0)) -10)
  (check-equal? (calc-newvx (make-circ 39 100 -10 -20 #f empty #f 0 0)) 10))
;____________________________________________________________________________________________________________________  
;;Calc-newvy : c -> Integer
;;GIVEN: Circle
;;RETURNS: Value of new vy after tick
;;STRATEGY: cases on vy 
;;EXAMPLES:
;;(calc-newvy (new-circle 200 100 10 -20)) => 210
;;(calc-newvy (new-circle 40 100 10 -20)) => 30
;;(calc-newvy (new-circle 360 100 10 -20)) => 350  
  (define (calc-newvy c)
   (cond
      [(y-in-middle? (circ-y c)) (circ-vy c)]  
      [else (* -1 (circ-vy c))]))
(begin-for-test
  (check-equal? (calc-newvy (make-circ 100 100 10 -20 #f empty #f 0 0)) -20)
  (check-equal? (calc-newvy (make-circ 50 39 -10 -20 #f empty #f 0 0)) 20)
  (check-equal? (calc-newvy (make-circ 60 261 -10 20 #f empty #f 0 0)) -20))

;____________________________________________________________________________________________________________________    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unpaused-world-after-tick : WorldState -> WorldState
;; GIVEN : WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick when unpaused.
;; STRATEGY : Use template for WorldState on w
;;Examples:
;;(unpaused-world-after-tick (make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f reddot))
;;=>(make-worldstate (new-circle 200 100 -12 20)(new-circle 200 200 23 -14) #f reddot)

(define (unpaused-world-after-tick w)
(cond
  [(reddot-clicked? (worldstate-reddot w)) w ]
  [else(make-worldstate(loc-after-tick (worldstate-loc w))
             (worldstate-paused? w)
             (worldstate-reddot w)
             (worldstate-clear? w))]))


(begin-for-test(check-equal?(unpaused-world-after-tick(make-worldstate (list (make-circ 200 150 0 0 #t '() #false 1 1)) #true
                                                                                      (make-reddot 209 173 #t) #false))(make-worldstate (list (make-circ 200 150 0 0 #t '() #false 1 1)) #true
                                                                                      (make-reddot 209 173 #t) #false)))



                                              

;____________________________________________________________________________________________________________________  
;;circle-after-tick: circle
(define (dot-after-tick x y)
  (make-dot x y #t))

(define(dots-in-circle x y lod)
  ( cons(dot-after-tick x y) lod))

  
;____________________________________________________________________________________________________________________  
;;circle-after-tick: circle
;;GIVEN: circle
;;RETURN : Circle
;;EXAMPLE: (circle-after-tick(make-circ 200 100 10 -20 #f empty #f 0 0))(make-circ 210 80 10 -20 #f empty #f 0 0)
;;STRATEGY: Use of template c on dot?
(define (circle-after-tick c)
  (cond [(circ-dot? c)(make-circ (calc-newx c)(calc-newy c) (calc-newvx c)(calc-newvy c) (circ-selected? c)
                           (dots-in-circle (calc-newx c) (calc-newy c) (circ-lod c)) (circ-dot? c) (circ-dx c) (circ-dy c))]
  [else (make-circ (calc-newx c)(calc-newy c) (calc-newvx c)(calc-newvy c) (circ-selected? c) (circ-lod c) (circ-dot? c) (circ-dx c) (circ-dy c))]))
(begin-for-test(check-equal?(circle-after-tick(make-circ 200 100 10 -20 #f empty #f 0 0))(make-circ 210 80 10 -20 #f empty #f 0 0)))
(begin-for-test(check-equal?(circle-after-tick(make-circ 200 100 10 -20 #f (list(make-dot 200 100 #t)) #t 0 0))
                            (make-circ 210 80 10 -20 #f(list(make-dot 210 80 #t)(make-dot 200 100 #t)) #t 0 0)))


;;loc-after-tick: loc-> loc
;;given : loc
;;returns: new positions of each circle in the list
;;examples:(listOfcircles-after-tick (cons (new-circle 200 100 -12 20) empty))=> list(new-circle 200 100 -12 20)
;STRATEGY: Use template for LOC 
(define(loc-after-tick loc)
  (cond
    [(empty? loc) empty]
    [else  (cons(circle-after-tick (first loc)) (loc-after-tick (rest loc)))]))
(begin-for-test(check-equal?(loc-after-tick(list(make-circ 200 100 10 -20 #f (list(make-dot 200 100 #t)) #t 0 0)))
                                              (list(make-circ 210 80 10 -20 #f (list(make-dot 210 80 #t)(make-dot 200 100 #t)) #t 0 0))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

;;x-in-middle?: x -> Boolean
;;GIVEN: value of x Integer
;;RETURNS: Boolean after checking if circle's x in middle or not, returns true if circle's x not touching boundary on y axis 
;;EXAMPLES:
;;(x-in-middle? 41) => #f
;;(x-in-middle? 359) => #t
;;(x-in-middle? 361) => #f
;;(x-in-middle? 39) => #t             
;;STRATEGY: Combine simpler functions

(define (x-in-middle? x)
  (and (> x RADIUS)(< x C-HITTING-BACKY-AXIS)))

(begin-for-test
  (check-equal? (x-in-middle? 41) #t "if x is 41 then circle hitting the y axis")
  (check-equal? (x-in-middle? 359) #t "if x is 359 then circle in middle")
  (check-equal? (x-in-middle? 361) #f "if x is 361 then circle hitting the y axis")
  (check-equal? (x-in-middle? 39) #f "if x is 41 then circle in middle"))
  

;;y-in-middle?: y -> Boolean
;;GIVEN: value of y Integer
;;RETURNS: Boolean after checking if circle's x in middle or not, returns true if circle's y not touching boundary on x axis 
;;EXAMPLES:
;;(y-in-middle? 39) => #f
;;(y-in-middle? 41) => #t
;;(y-in-middle? 30) => #t             

(define (y-in-middle? y)
  (and (> y RADIUS) (< y C-HITTING-LOWX-AXIS)))
(begin-for-test
  (check-equal? (y-in-middle? 41) #t "if x is 41 then circle hitting the y axis")
  (check-equal? (y-in-middle? 259) #t "if x is 359 then circle in middle")
  (check-equal? (y-in-middle? 260) #f "if x is 361 then circle hitting the y axis")
  (check-equal? (y-in-middle? 39) #f "if x is 41 then circle in middle"))


;;circle-middle?: c-> Boolean
;;GIVEN: Circle
;;RETURNS: true iff circle in the middle region of canvas and not hitting any axis
;;EXAMPLES:
(define (mycircle1 a) (new-circle 41 259 -12 20))
(define (mycircle2 b) (new-circle 40 260 -12 20))
;;(circle-middle c)

(define (circle-middle? c)
  ( and (x-in-middle? (circ-x c))(y-in-middle? (circ-y c))))

(begin-for-test
  (check-equal? (circle-middle? (mycircle1 2)) #t)
  (check-equal? (circle-middle? (mycircle2 2)) #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; circle-pen-down? : Circle -> Boolean
;;GIVEN: circle
;; RETURNS: true if the pen in the given circle is down
(define (circle-pen-down? c)
  (circ-dot? c))
;  )

  (begin-for-test(check-equal?(circle-pen-down? (make-circ 200 200 23 -14 #t empty #t -1 -1)) #t))

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: use template for Worldstate on w
(define (world-after-mouse-event w mx my mev)
  (make-worldstate
    (loc-after-mouse-event (worldstate-loc w) mx my mev)
    (worldstate-paused? w)
    (reddot-after-mouse-event (worldstate-reddot w) mx my mev)
    (worldstate-clear? w))
    )
;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: use template for Worldstate on w
(define (loc-after-mouse-event circles mx my mev)
  (cond
    [(empty? circles) empty]
    [else (cons (circ-after-mouse-event (first circles) mx my mev)(loc-after-mouse-event (rest circles) mx my mev))]
    ))
  

;; circl-after-mouse-event : circl Integer Integer MouseEvent -> circl
;; GIVEN: a circl and a description of a mouse event
;; RETURNS: the circl that should follow the given mouse event

;; strategy: use template for MousEevent mev
(define (circ-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "button-down") (circ-after-button-down c mx my)]
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
;; reddot-after-button-up : reddot Integer Integer -> reddot
;; GIVEN: a reddot and a description of a mouse
;; RETURNS: the reddot that should follow button-down
;; strategy: combine simpler functions
  (define (reddot-after-button-up mx my)
  ( make-reddot mx my #f)
    )

;; reddot-after-drag : Integer Integer -> reddot
;; GIVEN: a reddot and a description of a mouse
;; RETURNS: the reddot that should follow button-down
;; strategy: combine simpler functions
(define (reddot-after-drag mx my)
  (make-reddot mx my #t)
     )


;;loc-after-button-down: circles mx my -> loc
;;given: loc, mouse point
;;returns: loc with selected circles with selected? as #t
;;example: locirc-after-button-down
(define (loc-after-button-down circles mx my)
  (cond[(empty? circles) empty]
       [else (cons(circ-after-button-down (first circles) mx my) (loc-after-button-down (rest circles) mx my))]
   ))

;;loc-after-drag: circles mx my -> loc
;;given: loc, mouse point
;;returns: loc with selected circles with selected? as #t
;;example: locirc-after-button-down
(define (loc-after-drag circles mx my)
  (cond[(empty? circles) empty]
       [else(cons(circ-after-drag (first circles) mx my) (loc-after-drag (rest circles) mx my)) ]
   ))



;;loc-after-button-up: circles mx my -> loc
;;given: loc, mouse point
;;returns: loc with selected circles with selected? as #t
;;example: locirc-after-button-down
(define (loc-after-button-up circles mx my)
  (cond[(empty? circles) empty]
       [else (cons(circ-after-button-up (first circles) mx my) (loc-after-button-up (rest circles) mx my))]
   ))
(define selected-circle2a-at-200-200 (make-circ 200 200 23 -14 #t empty #f 0 0))
(define unselected-circle2a-at-200-200 (make-circ 200 200 23 -14 #f empty #f 0 0))
(define set-of-circles23 (cons selected-circle2a-at-200-200(cons unselected-circle9-at-100-200
                                                                 (cons unselected-circle10-at-200-300 empty))))
(define set-of-circles24 (cons unselected-circle2a-at-200-200(cons unselected-circle9-at-100-200
                                                                 (cons unselected-circle10-at-200-300 empty))))

(begin-for-test (check-equal?(loc-after-button-up set-of-circles23 201 201)set-of-circles24))



;; circl-after-button-down : circl Integer Integer -> circl
;; RETURNS: the circl following a button-down at the given location.
;; STRATEGY: Use template for circl on c
(define (circ-after-button-down c mx my)
  (if (in-circl? c mx my)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)  true (circ-lod c)(circ-dot? c) (-(circ-x c) mx) (-(circ-y c) my))
      c))
(begin-for-test (check-equal? (circ-after-button-down unselected-circle22-at-200-200 201 201)(make-circ 200 200 23 -14 #t empty #f -1 -1)))
(begin-for-test (check-equal? (circ-after-button-down unselected-circle22-at-200-200 400 400)(make-circ 200 200 23 -14 #f empty #f 0 0)))
;; circl-after-drag : circl Integer Integer -> circ
;; GIVEN: circle mousex mousey
;; RETURNS: the circl following a drag at the given location
;; STRATEGY: Use template for circl on c
(define (circ-after-drag c mx my)
  (if (circ-selected? c)
      (make-circ (+ mx (circ-dx c)) (+ my (circ-dy c)) (circ-vx c) (circ-vy c) #t (circ-lod c)(circ-dot? c) (circ-dx c) (circ-dy c))
      c))


(define selected-circle22-at-200-200 (make-circ 200 200 23 -14 #t empty #f 0 0))
(define selected-circle22-drag-at-200-200 (make-circ 10 10 23 -14 #t empty #f 0 0))
(begin-for-test(check-equal?(circ-after-drag selected-circle22-at-200-200 10 10)selected-circle22-drag-at-200-200 ))
(begin-for-test(check-equal?(circ-after-drag unselected-circle22-at-200-200 10 10)unselected-circle22-at-200-200 ))

;; circl-after-button-up: circl Integer Integer -> circl
;; GIVEN: circle mousex mousey
;; RETURNS: the circl following a button-up at the given location
;; STRATEGY: Use template for circl on c
;;EXAMPLES:
(define (circ-after-button-up c x y)
  ( cond
     [(circ-selected? c)
      (make-circ(circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false (circ-lod c)(circ-dot? c) (circ-dx c) (circ-dx c))]
      [else c]))


(define unselected-circle22-at-200-200 (make-circ 200 200 23 -14 #f empty #f 0 0))

(begin-for-test(check-equal?(circ-after-button-up selected-circle22-at-200-200 210 300)unselected-circle22-at-200-200 ))
(begin-for-test(check-equal?(circ-after-button-up unselected-circle22-at-200-200 210 300)unselected-circle22-at-200-200 ))  

;; in-circl?: circl Integer Integer -> Boolean
;; GIVEN: circle mousex mousey
;; RETURNS: true iff the given coordinate is inside the bounding box of
;; the given circle.
;; EXAMPLES: 
;; STRATEGY: Use template for circl on c
(define (in-circl? c x y)
  (<= (+ (sqr(- x (circ-x c)))
         (sqr(- y (circ-y c)))) (sqr 40) ))



;; world-circles : WorldState -> ListOfCircle
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.
(define (world-circles w)
   (worldstate-loc w))

(begin-for-test(check-equal? (world-circles (make-worldstate (list (make-circ 200 150 0 0 #t '() #false 1 1)) #true (make-reddot 209 173 #false) #false))
                             (list(make-circ 200 150 0 0 #t empty #false 1 1))))


;; circle-after-key-event : Circle KeyEvent -> Circle
;; RETURNS: the state of the circle that should follow the given
;; circle after the given key event

;; circle-pen-down? : Circle -> Boolean
;; RETURNS: true if the pen in the given circle is down
;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy).

(define (new-circle x y vx vy)
  (make-circ x y vx vy #f empty #f 0 0))
;______________________________________
;Test:
(begin-for-test(check-equal? (new-circle 10 20 -12 24)(make-circ 10 20 -12 24 #f empty #f 0 0)))
;####################################
;;world-paused? : WorldState -> Boolean
;;Given: worldState
;;RETURNS: true iff w is paused
;;EXAMPLES: (world-paused? (make-worldstate (list (make-circ 200 150 0 0 #t '() #f 1 1)) #f (make-reddot 209 173 #false) #false))=> #f
(define (world-paused? w)
  (worldstate-paused? w))
(begin-for-test (check-equal? (world-paused? (make-worldstate (list (make-circ 200 150 0 0 #t '() #f 1 1)) #f (make-reddot 209 173 #false) #false)) #f))