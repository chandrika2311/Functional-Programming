#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require "WidgetWorks.rkt")
(require 2htdp/image)
(check-location "10" "q1.rkt")

(provide
 make-metatoy
 run
 make-throbber
 make-clock
 make-politician
 Metatoy<%>
 Toy<%>
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Aim: Your boss at the toy factory has been taking PDP, and he has been persuaded to buy a "framework"
from WidgetWorks International. The salesman told him that the WidgetWorks framework eliminates the
need for your developers to deal with big-bang. You, of course, know that's not so hard, but your
boss, still hungover from his party, is pretty gullible. The salesman explains that with WidgetWorks,
call you do is create a WidgetWorks Container(TM), load it up with your widgets and swidgets,
and send it a 'run' message. You can even add additional widgets and swidgets while it's running.
Your boss is convinced, and tells you to reimplement your Metatoy using the WidgetWorks framework.
The framework is delivered as a file called WidgetWorks.rkt that provides three interfaces and one
function, as follows:|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define INITIAL-STATE 0)

;;Canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define  INITIAL-X (/ CANVAS-WIDTH 2))
(define  INITIAL-Y (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;Key Events
(define THROBBER-EVENT "t")
(define CLOCK "c")
(define CLOCK-EVENT "c")
(define POLITICIAN-EVENT "p")
(define THROBBER-COLOR "green")

;;Throbber
(define SOLID "solid")
(define OUTLINE "outline")
(define RADIUS-INCREMENT 1)
(define INITIAL-RADIUS 5)
(define MAXIMUM-RADIUS 20)

;;Clock
(define T-RADIUS-MAX 20)
(define T-RADIUS-MIN 5)
(define COUNT-INCREASE 1)
(define CLOCK-WIDTH 60)
(define  CLOCK-HEIGHT 30)
(define CLOCK-COLOR "red")
(define FILL "solid")
(define CLOCK-FONT 12)
(define CLOCK-FONT-COLOR "black")

;;Politician
(define hillary-image (bitmap "hillary60.jpg"))
(define replusion-distance 75)
(define speed-1 10)
(define speed-2 50)
(define trump-image (bitmap "trump60.jpg"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A Time is a NonNegative Integer

;; A Widget is an object whose class implements the Widget<%>
;; interface. 

;; A World is an object whose class implements the Metatoy<%>
;; interface.
;;A Throbber, Clock and Politician is an object whose class implements the Toy<%> interface


;; A KeyEvent is one of :
;; - "c"
;; - "p"
;; - "t"

;; INTERPRETATION
;; on pressing c, a new Clock toy appears
;; on pressing t, a new Throbber toy appears
;; on pressing p, a new Politician toy appears

;; TEMPLATE :
;; kev-fn : KeyEvent -> ??
;(define (kev-fn k)
;  (cond
;    [(string=? k "c")....]
;    [(string=? k "t")....]
;    [(string=? k "p")....]
;    [else...]))

;; A ListOfToy is one of :
;; -empty
;; -(cons Toy ListOfToy)

;; TEMPLATE :
;; lot-fn : ListOfToy -> ??
;(define (lot-fn lotoy)
;  (cond
;    [(empty? lot)...]
;    [else (....(first lotoy)
;               (lot-fn (rest lotoy)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACES
;; big-bang will communicate with the MetaToy with Metatoy<%>
;; interface which inherits from SWidget<%> interface.
(define Metatoy<%> 
  (interface
      ;; The interface Metatoy<%> inherits from the interface SWidget<%>.
      ;; This means that any class that implements Metatoy<%> must implement
      ;; all the methods from SWidget<%> plus all the methods defined here.
      (SWidget<%>)
    
    ;; -> ListOfToy
    get-toys))


;; Every object that lives in the Toy implements the SWidget<%>
;; interface.
(define Toy<%> 
  (interface
      
      ;; The interface Toy<%> inherits from the interface SWidget<%>.
      ;; This means that any class that implements Toy<%> must implement
      ;; all the methods from SWidget<%> plus all the methods defined here.
      (SWidget<%>)
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
run : PosNum -> Void
GIVEN: a frame rate (in seconds/tick)
EFFECT: This function creates a Container, and places a MetaToy with
no toys in that Container.  The function may or may not put other
Widgets and SWidgets in the container, depending on the
implementation. The function then runs the Container at the given
frame rate using WidgetWorks.|#
;; EXAMPLES:
;; (run 0.5 ) -> Starts the simulation at a frame rate 0.5, and
;;                          returns (object:Container% ...) at the end of it
;;STRATEGY:Combine Simpler Functions
(define (run rate)
  (local
    ((define the-world
       (container-init CANVAS-WIDTH CANVAS-HEIGHT))
     (define meta-toy (new Metatoy%[sobjs empty][container the-world])))
    (begin
      (send the-world add-stateful-widget meta-toy)
      (send the-world run rate))))


;;________________________________________________________________________
;;________________________________________________________________________
;;CLASSES:
;;________________________________________________________________________
;;________________________________________________________________________
;; Constructor template for Metatoy%: 
;; (new Metatoy% [objs sobjs])
;; Interpretation: An object of class Metatoy% takes signals from
;; big-bang and distributes them to its stateless and statefull objects as appropriate.
(define (make-metatoy sobjs)
  (new Metatoy% [sobjs sobjs][container container-init]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;WORLD CLASS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Metatoy%
  (class* object% (Metatoy<%>)
    
    
    (init-field sobjs);;List Of Stateful Toys
    (init-field container);Container
    
    (super-new)
    ;;____________________________________________________    
    ;; -> ListOfToys
    (define/public (get-toys)
      sobjs)
    ;;____________________________________________________   
    ;; after-tick :ListOfToys -> Void
    ;; EFFECT:updates this swidget to the state it should have following a tick.
    ;; EXAMPLES:
    ;; (send Toys after-tick) ->(includes states of all the toys in Container) after a tick
    ;; STRATEGY: Use of HOF map on ListOfToy
    (define/public (after-tick)
      (for-each
       ;; Toy<%> -> Toy<%>
       ;; GIVEN: A Toy
       ;; RETURNS: A Toy as the given toy but with its state after one tick
       (lambda (obj) (send obj after-tick)) sobjs))
    ;;____________________________________________________
    ;;add-to-scene : Scene -> Scene
    ;;GIVEN: a scene
    ;;RETURNS: a scene like the given one, but with ListOfSWidgets
    ;;painted on it.
    (define/public (add-to-scene s)
      (foldr
       ;; Scene ->  Scene 
       ;; GIVEN: A Toy and a Scene
       ;; RETURNS: A Scene as given with the given SWidget painted on it
       (lambda (obj scene)
         (send obj add-to-scene scene)) EMPTY-CANVAS sobjs))
    ;;____________________________________________________
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a KeyEvent
    ;; EFFECT: updates this widget to the state it should have
    ;; following the given key event
    ;; EXAMPLES:
    ;;          (send THROBBER1 after-key-event THROBBER-EVENT) -> 
    ;;                                  creates a new throbber in the Container
    ;;          (send CLOCK1 after-key-event Clock-EVENT) ->
    ;;                                  creates a new clock  in the Container
    ;; STRATEGY: Cases on kev
    ;; "c", "t" and "p" create new Clock, new Throbber and new Politician respectively
    ;; other keystrokes are passed on to the objects in the Metatoy.
    
    (define/public (after-key-event ke)
      (cond
        [(key=? ke THROBBER-EVENT)
         (set! sobjs
               (cons (make-throbber INITIAL-X INITIAL-Y)sobjs))]
        [(key=? ke CLOCK-EVENT)
         (set! sobjs
               (cons (make-clock INITIAL-X INITIAL-Y) sobjs))]
        [(key=? ke POLITICIAN-EVENT)
         (set! sobjs
               (cons (make-politician)sobjs))]
        [else(set! sobjs
                   (map
                    (lambda (obj1)(send obj1 after-key-event ke))sobjs))]))
    
    ;;____________________________________________________
    ;; after-mouse-event : Integer Integer MouseEvent -> Metatoy
    ;; GIVEN: Two NonNegIntegers which represent the mouse position's x and y
    ;;        coordinates, and a MouseEvent mev
    ;; RETURNS: Updates this widget to the state it should have
    ;; following the given Mouse event
    ;; EXAMPLES:
    ;;          (send Toys after-mouse-event 315 210 MOUSE-DOWN-EVENT)) -> 
    ;;          selects the object(s) in the container inside which the mev occurred and places them 
    ;;          on the mouse point location
    ;;          (send Toys after-mouse-event 315 210 MOUSE-DRAG-EVENT) ->
    ;;          drags the object(s) in the container inside which the mev occurred
    ;; STRATEGY: Cases on mev
    
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (after-button-down mx my)]
        [(mouse=? mev "drag")
         (after-drag mx my)]
        [(mouse=? mev "button-up")
         (after-button-up mx my)]
        [(mouse=? mev "move")
         (after-move mx my)]
        ))
    ;;____________________________________________________
    ;;after-button-down : Integer Integer -> Void
    ;;Given: Mouse coordinates after button-down event
    ;;EFFECT: updates this widget to the state it should have
    ;; following the given button down event
    ;;Examples:
    ;; (send toys1 after-button-down 315 210)->
    ;;  creates a container after performing the button-down event on the selected
    ;;   toy at given mouse co-ordinates
    ;;Strategy: Use HOFC map on the ListOfSwidget objs
    
    (define/public (after-button-down mx my)
      (for-each (lambda (obj) (send obj after-button-down mx my)) sobjs))
    
    ;;____________________________________________________
    ;;after-button-up : Integer Integer -> Void
    ;;Given: Mouse coordinates after button-up event
    ;;EFFECT:updates this widget to the state it should have
    ;;following the given button up event
    ;;Examples:
    ;; (send Toys world-after-button-up 315 210)->
    ;;  creates a Container after performing the button-up event on the selected
    ;;   toy at given mouse co-ordinates
    ;;Strategy: Use HOFC map on the ListOfSWidgets objs
    (define/public (after-button-up mx my)
      (for-each (lambda (obj) (send obj after-button-up mx my))sobjs))
    
    ;;____________________________________________________
    ;;after-drag : Integer Integer -> Void
    ;;Given: Mouse coordinates after drag event
    ;;EFFECT:updates this widget to the state it should have
    ;;following the given button up event
    ;;Examples:
    ;; (send Toys after-button-drag 315 210)->
    ;;  creates a container after performing the button-drag event on the selected
    ;;   toy at given mouse co-ordinates
    ;;Strategy: Use HOFC map on the ListOfSWidgets objs
    (define/public (after-drag mx my)
      (for-each (lambda (obj) (send obj after-drag mx my))sobjs))
    ;;____________________________________________________
    ;;after-move : Integer Integer -> Void
    ;;Given: Mouse coordinates after move event
    ;;Returns: A World after move event
    ;;Examples:
    ;; (send Toys1 world-after-move 315 210)->
    ;;  creates a Container after performing the move event on the selected
    ;;   toy at given mouse co-ordinates    
    ;;Strategy: Use HOFC map on the ListOfSWidgets objs
    
    (define/public (after-move mx my)
      (for-each (lambda (obj) (send obj after-move mx my))sobjs))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CLOCK CLASS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor template for Clock%: 
;; (new CLock% [ x y second-count INITIAL-STATE])
;;Interpretation: An object of a class Clock% represents the clock toy
;; Clock is created at the centre of the canvas. The timer increases
;; on each tick 
(define (make-clock x y)
  (new Clock% [x x][ y y] [second-count INITIAL-STATE]))

(define Clock%
  (class* object% (Toy<%>)
    ;; the x and y positions of the center of the throbber and a counter which is
    ;;initially 0 and increases by 1 with each tick 
    (init-field x y second-count)
    ;; current mouse pointer locations
    (init-field [curr-mx INITIAL-STATE] [curr-my INITIAL-STATE])
    ;;selected? determines if the clock is selected by mouse down event, initially false
    (init-field [selected? false])
    ;; If throbber selected then outline else solid
    (field [height CLOCK-HEIGHT])
    (field [width CLOCK-WIDTH])
    (field [COLOR CLOCK-COLOR])
    ;; image for displaying the Clock
    (field [CLOCK-PIC (overlay
                       (text (format "~a" second-count)
                             CLOCK-FONT
                             CLOCK-FONT-COLOR)
                       (rectangle  CLOCK-WIDTH CLOCK-HEIGHT FILL CLOCK-COLOR))])
    
    
    (super-new)
    ;;___________________________________________________
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing clock location's x-coordinate value
    ;; EXAMPLES: (send Clock1 toy-x) -> 300
    (define/public (toy-x) x)
    
    ;;___________________________________________________
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing clock location's y-coordinate value
    ;; EXAMPLES: (send Clock1 toy-y) -> 250
    (define/public (toy-y) y)
    
    ;;___________________________________________________
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing clock location's second count value
    ;; EXAMPLES: (send Clock1 toy-data) -> 0
    (define/public (toy-data) second-count)
    
    ;;___________________________________________________
    ;; after-tick : -> Void
    ;; RETURNS: Updates the clock to look how it should be after a tick
    ;; DETAILS: the timer increases by 1 at each tick.
    ;; EXAMPLES:
    ;; (send clock1 after-tick) -> Updates Clock1 state after 1 tick the timer counter
    ;;                              incraesed by 1        
    (define/public (after-tick)
      (begin
        (set! x x)
        (set! y y)
        (set! second-count (+ 1 second-count))
        (set! selected? true)
        (set! curr-mx curr-mx)
        (set! curr-my curr-my))this)
    ;;___________________________________________________
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: A Scene
    ;; RETURNS: a Scene like the given one, but with this Clock painted
    ;;          on it
    ;; EXAMPLES: (send CLOCK1 add-to-scene EMPTY-CANVAS) -> CLOCK-SCENE
    ;; DESIGN STRATEGY: Combine simpler functions.
    (define/public (add-to-scene s)
      (place-image (overlay
                    (text (format "~a" second-count)
                          CLOCK-FONT
                          CLOCK-FONT-COLOR)
                    (rectangle  CLOCK-WIDTH CLOCK-HEIGHT FILL CLOCK-COLOR)) x y s))
    
    ;;___________________________________________________
    ;; after-key-event :  KeyEvent -> Clock
    ;; GIVEN: a KeyEvent kev
    ;; RETURNS: A Worldlike like given, but after the KeyEvent occurred
    ;; EXAMPLES:
    ;;          (send clock1 after-key-event  "c")) -> clock1
    ;;           no changes
    (define/public (after-key-event kev) this)
    ;;___________________________________________________
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the coordinates of a button-down event
    ; EFFECT: Updates A clock after performing the button down event
    ; Examples:
    ; (send Clock1 after-button-down 315 210)->
    ;  updates a Clock after performing the button-down event on the selected
    ;;   Clock at given mouse co-ordinates
    ; STRATEGY: Cases on whether the event is in the clock.
    ; If the mouse event occurs withing the clock, then selected? becomes true
    (define/public (after-button-down mx my)
      (if (check-in-clock? mx my)
          (begin
            (set! x x)
            (set! y y)
            (set! second-count second-count)
            (set! selected? true)
            (set! curr-mx (- mx x))
            (set! curr-my (- my y)))
          42))
    ;;___________________________________________________
    ; check-in-clock? : Integer Integer -> Boolean
    ; GIVEN: the coordinates of a mouse pointer
    ; Returns; true iff the mouse pointer is within the clock
    ; EXAMPLES:
    ; (check-in-clock? CLOCK1 301 251) -> true
    ; STRATEGY: combine simpler functions
    (define (check-in-clock? x1 y1)
      (and(<= (abs (- x1 x)) CLOCK-WIDTH)
          (<= (abs (- y1 y)) CLOCK-HEIGHT)))
    ;;___________________________________________________
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the location of a button-up event
    ; EFFECT: Updates A clock after performing the button up event
    ; STRATEGY: Cases on whether the event is in the clock.
    ; EXAMPLES:
    ; (send Clock1 after-button-up 315 210)->
    ;  Updates a Clock1 after performing the button-up event on the selected
    ; clock at given mouse co-ordinates
    ; If the clock is selected, then unselect it.
    (define/public (after-button-up mx my)
      (begin
        (set! x x)
        (set! y y)
        (set! second-count second-count)
        (set! selected? false)
        (set! curr-mx curr-mx)
        (set! curr-my curr-my))this)
    ;;___________________________________________________
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; Effect: Updates A clock to the state of the clock after drag event
    ; Examples:
    ; (send Clock after-drag 315 210)->
    ;  updates Clock after performing the button-drag event on the selected
    ;   clock at given mouse co-ordinates 
    ; STRATEGY: Cases on whether the event is in the clock.
    ; If the clock is selected, then drag it.
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx curr-mx))
            (set! y (- my curr-my))
            (set! second-count second-count)
            (set! selected? true)
            (set! curr-mx curr-mx)
            (set! curr-my curr-my))
          this))
    
    ;; after-move: Integer Integer -> Void
    ;;Given: Mouse coordinates
    ; Effect: Updates A clock to the state of the clock after move event
    ;;Examples:
    ;; (send Clock1 after-move 315 210)-> creates same Clock1 
    (define/public (after-move mx my)
      this)
    
    (define/public (for-test:toy-x) x)
    (define/public (for-test:toy-y) y)
    (define/public (for-test:toy-data) second-count)
    ))


;;___________________________________________________
;;___________________________________________________
;;___________________________________________________
;;___________________________________________________
(define CLOCK11 (make-clock 100 300))
(define CLOCK21 (new Clock% [x 400][y 500] [second-count INITIAL-STATE]))



(begin-for-test
  (local
    ;;creates a clock:
    ((define CLOCK1 (new Clock% [x 100][y 300][curr-mx 0][curr-my 0][second-count INITIAL-STATE]))
     (define CLOCK1-BUTTON-DOWN
       (new Clock% [x 102][y 302] [curr-mx 2][curr-my 2][second-count INITIAL-STATE]))
     (define CLOCK-SCENE(send CLOCK1 add-to-scene EMPTY-CANVAS)))
    
    (check-equal?(send CLOCK1 for-test:toy-x) 100)
    (check-equal?(send CLOCK1 for-test:toy-y) 300)
    (check-equal?(send CLOCK1 for-test:toy-data) 0)
    (check-equal? (send CLOCK1 add-to-scene EMPTY-CANVAS)CLOCK-SCENE)
    (check-equal? (send CLOCK1 after-move 20 20)CLOCK1)
    (check-equal? (send CLOCK1 after-drag 20 20)CLOCK1)
    (send CLOCK1 after-tick)
    (send CLOCK1 after-button-down 102 300)
    (send CLOCK1 after-button-down 10 30)
    (send CLOCK1 after-drag 102 205)
    (send CLOCK1 after-button-up 102 205)
    (send CLOCK1 after-key-event "c")
    (check-equal?
     (send CLOCK1 toy-x) 100 "the x co-ordinate of the toy extracted")
    (check-equal?
     (send CLOCK1 toy-y) 205 "the y co-ordinate of the toy extracted")
    (check-equal?
     (send CLOCK1 toy-data) 1 "Time shown by clock")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;THROBBER CLASS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Throbber is created on the centre of the canvas. Its size increases at each tick
;; upto 20 pixel radius after which it decreases at each tick to 5 pixel radius.
;;This process goes on

;; Constructor template for Throbber%
;; (new Throbber% [x Integer] [y Integer])
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
;; EXAMPLES:(make-throbber 300 250)-> Creates a throbber at the center of the
;;          canvas which incraeses it radius until 20 and decreases in size
;;          until radius is 5
;; STRATEGY:
(define (make-throbber x y)
  (new Throbber% [x x][ y y]))
;;___________________________________________________
(define Throbber%
  (class* object% (Toy<%>)
    
    ;; the x and y positions of the center of the throbber
    (init-field x y)
    
    ;; the radius of the throbber at the start
    (init-field [radius INITIAL-RADIUS])
    
    ;;Increasing keeps track if the throbber is increasing,
    ;;#t iff the throbber is increasing in size
    (init-field [increasing true])
    ;;Selected? keeps track if the throbber is selected,
    ;;#t iff the throbber is selected by button
    ;;down event
    (init-field [selected? false])
    
    ;; current mouse pointer coordinates
    (init-field [curr-mx INITIAL-STATE] [curr-my INITIAL-STATE])
    
    ;; If throbber selected then outline else solid
    (init-field [fill SOLID])
    
    ;;Initial radius of the throbber, also minimum radius of the throbber
    (field [MIN-RADIUS INITIAL-RADIUS])
    ;;Maximum radius of the throbber
    (field [MAX-RADIUS MAXIMUM-RADIUS])
    ;;Color of the throbber
    (field [COLOR THROBBER-COLOR])
    (field [THROBBER-PIC (circle radius fill COLOR)]) 
    
    (super-new)
    ;;___________________________________________________
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing throbber location's x-coordinate value
    ;; EXAMPLES: (send THROB1 toy-x) -> 300
    (define/public (toy-x) x)
    ;;___________________________________________________
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing throbber location's y-coordinate value
    ;; EXAMPLES: (send THROB1 toy-y) -> 250
    (define/public (toy-y) y)
    ;;___________________________________________________
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing throbber location's y-coordinate value
    ;; EXAMPLES: (send THROB1 toy-data) -> 5
    (define/public (toy-data) radius)
    
    ;;___________________________________________________
    ;; after-key-event :  KeyEvent -> Throbber
    ;; GIVEN: a KeyEvent mev
    ;; RETURNS: A Worldlike like given, but after the KeyEvent occurred
    ;; EXAMPLES:
    ;;          (send THROB1 after-key-event  "c")) -> THROB1
    ;;           no changes  
    (define/public (after-key-event kev) this)
    ;;___________________________________________________
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the coordinates of a button-down event
    ; EFFECT: Updates A Throbber's state to its state after the button down event has been implemented
    ; Examples:
    ; (send THROB1 after-button-down 315 210)->
    ;  Updates THROB1 after performing the button-down event on the selected
    ;;   Throbber at given mouse co-ordinates
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the mouse event occurs withing the throbber, then selected? becomes true.
    (define/public (after-button-down mx my)
      (if (check-in-throbber? mx my) 
          (begin
            (set! x x)
            (set! y y)
            (set! radius radius)
            (set! selected? true)
            (set! increasing increasing)
            (set! curr-mx (- mx x))
            (set! curr-my (- my y))
            (set! fill OUTLINE))50))
    ;;___________________________________________________
    ; check-in-throbber? : Integer Integer -> Boolean
    ; GIVEN: the coordinates of a mouse pointer
    ; Returns; true iff the mouse pointer is within the throbber
    ; EXAMPLES:
    ; (check-in-throbber? THROB1 301 251) -> true
    ; STRATEGY: combine simpler functions
    
    (define (check-in-throbber? x1 y1)
      (<= (+ (sqr (- x x1))(sqr (- y y1))) (sqr radius)))
    ;;___________________________________________________
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the location of a button-up event
    ; EFFECT: Updates A Throbber's state to its state after the button up event has been implemented 
    ; EXAMPLES:
    ; (send THROB1 after-button-up 315 210)->
    ;  Updates a THROB1 after performing the button-up event on the selected
    ;   Throbber at given mouse co-ordinates
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the throbber is selected, then unselect it.
    (define/public (after-button-up mx my)
      (begin   (set! x x)
               (set! y y)
               (set! radius radius)
               (set! selected? false)
               (set! increasing increasing)
               (set! curr-mx curr-mx)
               (set! curr-my curr-my)
               (set! fill SOLID)))
    ;;___________________________________________________
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; EFFECT: Updates A Throbber's state to its state after drag event has been implemented
    ; Examples:
    ; (send THROB1 after-drag 315 210)->
    ;  Updates A THROB1 after performing the after-drag event on the selected
    ;   Throbber at given mouse co-ordinates 
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the throbber is selected, then drag it.
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx curr-mx)) 
            (set! y (- my curr-my))
            (set! radius radius)
            this)45))
    ;;___________________________________________________
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: A Scene
    ;; RETURNS: a Scene like the given one, but with this throbber painted
    ;;          on it
    ;; EXAMPLES: (send THROB1 add-to-scene EMPTY-CANVAS) -> THROBBER-SCENE
    ;; DESIGN STRATEGY: Combine simpler functions.
    
    (define/public (add-to-scene s)
      (place-image (circle radius fill COLOR) x y s))
    ;;___________________________________________________
    ;; after-tick : -> Void
    ;; EFFECT: Updates throbber to its state but how it should be after a tick
    ;; EXAMPLES:
    ;; (send THROB1 after-tick) -> THROB1 state after 1 tick the timer counter
    ;;                             with radius increased by 1 if the current
    ;;                             radius is <=20 else radius decreased by 1
    ;;                             if the current radius >=5
    ;; STRATEGY: Cases on the value of radius
    ;; DETAILS: the throbber radius increases by 1 at each tick till the radius is 20 after which
    ;;decreases by 1 till it reached radius 5.
    (define/public (after-tick)
      (if(or (and (< radius MAXIMUM-RADIUS) increasing)
             (<= radius INITIAL-RADIUS))
         (begin
           (set! x x)
           (set! y y)
           (set! radius (+ radius RADIUS-INCREMENT))
           (set! selected? selected?)
           (set! increasing true)
           (set! curr-mx curr-mx)
           (set! curr-my curr-my)
           (set! fill fill))
         (begin
           (set! x x)
           (set! y y)
           (set! radius (- radius RADIUS-INCREMENT))
           (set! selected? selected?)
           (set! increasing false)
           (set! curr-mx curr-mx)
           (set! curr-my curr-my)
           (set! fill fill))))
    ;;___________________________________________________  
    ;; after-move: Integer Integer -> Throbber    
    ;;Given: Mouse coordinates after move event
    ;;Returns: A Clock after move event
    ;;Examples:
    ;; (send THROB1 after-move 315 210)->  creates same THROB1 
    (define/public (after-move mx my)
      this)
    ))

;;___________________________________________________
(define THROB1 (make-throbber 300 250))
(define THROB-SCENE (place-image (circle 5 "solid" "green")
                                 300 250 EMPTY-CANVAS))
;;___________________________________________________
;;___________________________________________________
;;___________________________________________________
(begin-for-test
  (send THROB1 add-to-scene EMPTY-CANVAS)     
  (check-equal?
   (send THROB1 toy-x) 300 "the x co-ordinate of the toy extracted") 
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-tick)
  (send THROB1 after-drag 20 20)
  (send THROB1 after-button-down 10 10)
  (send THROB1 after-button-down 302 250)
  (send THROB1 after-drag 20 20)
  (send THROB1 after-button-up 20 20)
  
  (check-equal?
   (send THROB1 toy-y) 20 "the y co-ordinate of the toy extracted")
  
  (check-equal?
   (send THROB1 toy-data) 13 "the radius of the toy extracted")
  
  (check-equal?
   (send THROB1 after-key-event "x") THROB1
   "the radius of the toy extracted")
  
  
  
  (check-equal?
   (send THROB1 after-move 350 250) THROB1
   "the radius of the toy extracted"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Politician is created on the centre of the canvas. It is not draggable.
;; It follows the mouse cursor at each tick, If the politician comes within
;; 75 pixel radius of the mouse cursor, it bousces back and changes its image
;; before approaching again towards the mouse point.
;; Default movement of the Politician before any mouse event will be North from center

;; Constructor template for Politian%
;; (new Politician%([x Integer][y Integer][poli-x Integer][poli-y Integer][POLITICIAN-IMG bitmap])
;; GIVEN: an x and a y position
;; RETURNS: an object representing a Politician at the coordinates x y.
(define (make-politician)
  (new Politician%
       [x INITIAL-X][y INITIAL-Y]
       [poli-mx 250][poli-my 0]
       [POLITICIAN-IMG hillary-image]))
;;___________________________________________________
;;___________________________________________________
(define Politician%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one Politician to
    ;; the next.
    
    ; the x and y position of the center of the Politician
    (init-field x y)       
    (init-field poli-mx poli-my)
    (init-field POLITICIAN-IMG )                    
    (super-new)
    ;;___________________________________________________
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing throbber location's x-coordinate value
    ;; EXAMPLES: (send POLITICIAN toy-x) -> 250
    (define/public (toy-x) x)
    
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing throbber location's y-coordinate value
    ;; EXAMPLES: (send POLITICIAN toy-y) -> 250
    (define/public (toy-y) y)
    
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Distance between the politician and the mouse point after mouse move event
    ;; EXAMPLES: (send POLITICIAN toy-data) -> Distance between the toy and mouse pointer
    (define/public (toy-data)(round(calculate-hypo x y poli-mx poli-my)))
    ;;___________________________________________________
    ; after-move : Integer Integer ->Void
    ; GIVEN: the location of the mouse coordinates after mouse move event
    ;;EFFECT: Updates the new mouse points inside the politician
    ;;EXAMPLE: (send POLITICIAN after-move 20 30) -> Updated the POLITICIAN's state to after
    ;;the after-move event
    (define/public (after-move mx my)
      (begin
        (set! x x)
        (set! y y)
        (set! poli-mx mx)
        (set! poli-my my)
        (set! POLITICIAN-IMG POLITICIAN-IMG)))
    ;;___________________________________________________
    ;;calculate-hypo: Integer,Integer,Integer,Integer
    ;;GIVEN: x and y coordinates of politician and mouse
    ;;x, y: Politician center coordinates
    ;;mx,my: Mouse coordinates
    ;;RETURNS: distance between Politician's centre to Mouse coordinates
    ;;EXAMPLES: (calculate-hypo 2 3 4 5) => 2 
    (define (calculate-hypo x y mx my)
      (sqrt(+ (sqr(- mx x)) (sqr(- my y)))))
    ;;___________________________________________________
    ;;new-x:Integer,Integer,Integer,Integer-> Integer
    ;;GIVEN: x and y coordinates of politician and mouse
    ;;x, y: Politician center coordinates
    ;;mx,my: Mouse coordinates
    ;;RETURNS: New x coordinates of the Politician on the line
    ;;EXAMPLE: (new-x 2 4 5 3) => 2.4
    (define (new-x x y mx my)
      (/ (- mx x) (calculate-hypo x y mx my)))
    ;;___________________________________________________
    ;;new-y:Integer,Integer,Integer,Integer
    ;;GIVEN: x and y coordinates of politician and mouse
    ;;x, y: Politician center coordinates
    ;;mx,my: Mouse coordinates
    ;;RETURNS: New y coordinates of the Politician on the line
    ;;EXAMPLE: (new-y 2 4 5 3) => 2.4
    (define(new-y x y mx my)
      (/ (- my y) (calculate-hypo x y mx my)))
    ;;___________________________________________________
    ;; after-tick : -> Void
    ;; EFFECT: Updates A Politician's state to its state after a tick
    ;; DETAILS: the Politician comes one step closer to the mouse at each tick.
    ;;But repels from the mouse point if within radius of 75 pixels of the mouse. Repeats the process
    ;;EXAMPLE: (send POLITICIAN after-tick)=> updates POLITICIAN's state to a state after the tick
    (define/public (after-tick)
      (if (>= (calculate-hypo x y poli-mx poli-my) replusion-distance)
          (begin
            (set! x (+ x (* speed-1(new-x x y poli-mx poli-my))))
            (set! y (+ y (* speed-1(new-y x y poli-mx poli-my))))
            (set! poli-mx poli-mx)
            (set! poli-my poli-my)
            (set! POLITICIAN-IMG POLITICIAN-IMG))
          (begin
            (set! x (- x (* speed-2(new-x x y poli-mx poli-my))))
            (set! y (- y (* speed-2(new-y x y poli-mx poli-my)))) 
            (set! poli-mx poli-mx)
            (set! poli-my poli-my)
            (set! POLITICIAN-IMG
                  (if (equal? POLITICIAN-IMG hillary-image) trump-image hillary-image)))
          ))
    ;;___________________________________________________
    ;; after-key-event :  KeyEvent -> Politician
    ;; GIVEN: a KeyEvent kev
    ;; RETURNS: A Container like given, but after the KeyEvent occurred
    ;; EXAMPLES:
    ;;          (send Politician after-key-event  "p")) -> Politician
    ;;           no changes
    ;No effect on the Polotician on occurance of key event
    (define/public (after-key-event kev)
      this)      
    ;;___________________________________________________
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: A Scene
    ;; RETURNS: a Scene like the given one, but with this POLITICIAN-IMG painted
    ;;          on it
    ;; EXAMPLES: (send POLITICIAN-IMG add-to-scene EMPTY-CANVAS) -> POLITICIAN-SCENE
    ;; DESIGN STRATEGY: Combine simpler functions.
    (define/public (add-to-scene scene)
      (place-image POLITICIAN-IMG x y scene))
    ;;___________________________________________________
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the coordinates of a button-down event
    ; EFFECT: An updated Politician after performing the button down event
    ; Examples:
    ; (send Politician after-button-down 315 210)->
    ;  creates a Politician after performing the button-down event on the selected
    ;;   Politician at given mouse co-ordinates
    ; STRATEGY: Cases on whether the event is in the Politician.
    ; If the mouse event occurs withing the Politician, then selected? becomes true.
    (define/public (after-button-down mx my)
      (begin
        (set! x x)
        (set! y y)
        (set! poli-mx mx)
        (set! poli-my my)
        (set! POLITICIAN-IMG POLITICIAN-IMG)))
    ;;___________________________________________________
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; EFFECTS: An updated Politician after drag event
    ; Examples:
    ; (send Politician after-drag 315 210)->
    ;  creates a Politician after performing the button-drag event on the selected
    ;   Politician at given mouse co-ordinates 
    ; STRATEGY: Cases on whether the event is in the Politician.
    ; Drag has not effect on politician as the politician moves away from the mouse pointer
    (define/public (after-drag mx my)
      (begin
        (set! x x)
        (set! y y)
        (set! poli-mx mx)
        (set! poli-my my)
        (set! POLITICIAN-IMG POLITICIAN-IMG)))
    ;;___________________________________________________
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the location of a button-up event
    ; RETURNS: A clock after performing the button up event
    ; STRATEGY: Cases on whether the event is in the clock.
    ; EXAMPLES:
    ; (send Clock1 after-button-up 315 210)->
    ;  creates a clock after performing the button-up event on the selected
    ;   clock at given mouse co-ordinates
    ; If the clock is selected, then unselect it.
    (define/public (after-button-up mx my)
      (begin
        (set! x x)
        (set! y y)
        (set! poli-mx mx)
        (set! poli-my my)
        (set! POLITICIAN-IMG POLITICIAN-IMG)))
    
    ))
;;___________________________________________________
;;___________________________________________________
;;___________________________________________________
;;___________________________________________________
(begin-for-test
  (local
    ((define INIT-POLI (make-politician))
     (define INIT-POLI-DEFAULT (new Politician% [x INITIAL-X]
                                    [y INITIAL-Y]
                                    [poli-mx 0]
                                    [poli-my 0]
                                    [POLITICIAN-IMG hillary-image]))
     (define INIT-POLI1 (new Politician% [x 50]
                             [y 60]
                             [poli-mx 51]
                             [poli-my 61]
                             
                             [POLITICIAN-IMG hillary-image])))
    (send INIT-POLI1 after-tick)
    (send INIT-POLI1 after-tick)
    (send INIT-POLI1 after-tick)
    
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    (send INIT-POLI after-tick)
    
    (send INIT-POLI after-button-down 2 4)
    (send INIT-POLI after-button-down 250 300)
    (send INIT-POLI after-drag 250 300)
    (send INIT-POLI add-to-scene EMPTY-CANVAS)
    (send INIT-POLI after-button-up 23 20)
    (send INIT-POLI after-key-event "o")
    (send INIT-POLI after-move 23 20)
    
    (check-equal?
     (send INIT-POLI toy-x)
     INITIAL-X)
    (check-equal?
     (send INIT-POLI toy-y)
     180)
    (check-equal?
     (send INIT-POLI toy-data)
     278.0)
    
    ))
(begin-for-test
  (local
    
    ((define TOYS1 (make-metatoy empty))
     (define TOYS2  (make-metatoy
                     (list (make-clock 300 250)
                           (make-throbber 300 250)
                           (make-politician))
                     )))
    
    (send TOYS2 get-toys)      
    (send TOYS1 after-key-event THROBBER-EVENT) 
    (send TOYS1 after-key-event CLOCK-EVENT)
    (send TOYS1 after-key-event "t")
    (send TOYS1 after-key-event "c")
    (send TOYS1 after-key-event "x")
    (send TOYS1 after-key-event "p")
    (send TOYS1 after-mouse-event 310 250 "button-down")
    (send TOYS2 after-key-event THROBBER-EVENT) 
    (send TOYS2 after-key-event CLOCK-EVENT)
    (send TOYS2 after-key-event "t")
    (send TOYS2 after-key-event "c")
    (send TOYS2 after-key-event "x")
    (send TOYS2 after-key-event "p")
    (send TOYS2 after-mouse-event 310 250 "button-down")
    (send TOYS2 after-tick)
    (send TOYS2 after-button-down 300 250)
    (send TOYS2 after-button-up 300 250)
    (send TOYS2 after-mouse-event 300 250 "drag")
    
    (send TOYS2 after-mouse-event 300 250 "move")
    (send TOYS2 after-mouse-event 300 250 "button-up")
    (send TOYS2 add-to-scene EMPTY-CANVAS)
    
    ))

