#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start with (run framerate).  Typically: (run 0.25)
;;Program to enable Gullu our second grader to interact with digital minitoys
;;like throbber clock and politician image

;;When Gullu types "t", child can play with a new mini-toy called Throbber.
;;She can select and move the throbber using smooth drag.

;;When Gullu types "c", child can play with a new mini-toy called Clock.
;;She can select and move the clock using smooth drag.
;;This clock displays the number of ticks since it was created. 

;;When Gullu types "p", she can play with a new mini-toy called Politician.
;;The politician always moves in a straight line either towards the mouse
;;position or away from it.Politician cannot be dragged.

;;There is no interaction between clock, throbber and politician.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(check-location "09" "q1.rkt")

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

;;Throbber
(define SOLID "solid")
(define OUTLINE "outline")
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
;; interface which inherits World<%> interface.

(define World<%>
  (interface ()

    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick 
    after-tick

    ; Integer Integer MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event

    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event

    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

;; Every object in Toy<%> implements all methods in Widget<%>
;; interface
(define Widget<%>
  (interface ()

    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;; big-bang will communicate with the MetaToy with Metatoy<%>
;; interface.
(define Metatoy<%>
  (interface 
  
   ;; the (World<%>) says that Metatoy<%> inherits from World<%>
   ;; This means that any class that implements Metatoy<%> must
   ;; implement all the methods from World<%> plus all the methods
   ;; defined here. In this case, there is just one additional method,
   ;; called get-toys.
   (World<%>)

    ;; -> ListOfToy
    get-toys

))
;; Every object that lives in the Metatoy implements the Toy<%>
;; interface.
(define Toy<%> 
  (interface
  
   ;; The interface Toy<%> inherits from the interface Widget<%>.
   ;; This means that any class that implements Toy<%> must implement
   ;; all the methods from Widget<%> plus all the methods defined here.
   (Widget<%>)


    ;; Note: the Widgets of the space-invader-examples don't respond
    ;; to mouse "move" events, but some of our toys do.  So we add an
    ;; after-move method to the interface.

    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    after-move

 
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


;; initial-world : -> World
;; RETURNS: a world with an empty canvas
(define (initial-world)
  (make-world empty INITIAL-STATE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run : PosNum -> Metatoy
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: creates a MetaToy with no toys in it, and runs it using big-bang
;; at the given frame rate.  Returns the final state of the Metatoy.
;; EXAMPLES:
;; (run 0.5 ) -> Starts the simulation at a frame rate 0.5, and
;;                          returns (object:WorldState% ...) at the end of it
;; STRATEGY:
(define (run rate)
  (big-bang (initial-world) 
      (on-tick
       ;; 
       ;; GIVEN: a world 
       ;; RETURNS: a world as given but after a tick
       (lambda (w) (send w after-tick))rate)

       (on-draw
        ;;
        ;; GIVEN: a world 
        ;; RETURNS: a scene with this world drawn on it
        (lambda (w) (send w to-scene)))

         (on-key
         ;;
         ;; GIVEN: a world 
         ;; RETURNS: a world as given but after a key event
         (lambda (w kev) (send w after-key-event kev)))

         (on-mouse
         ;;
         ;; GIVEN: a world 
         ;; RETURNS: a world as given but after a mouse-event
         (lambda (w mx my mev)
                (send w after-mouse-event mx my mev)))))

;;________________________________________________________________________
;;________________________________________________________________________
;;CLASSES:
;;________________________________________________________________________
;;________________________________________________________________________

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;WORLD CLASS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define World%
  (class* object% (Metatoy<%>)

    (init-field obj) ;; ListOfToy
    (init-field time) ;; Time

    (super-new)

    ;; -> ListOfToy
    (define/public (get-toys)
      obj)

    ;; after-tick : -> World
    ;; RETURNS: A world like this one, but how it should be after a tick
    ;; DETAILS: the world timer increases by 1 at each tick.
    ;; EXAMPLES:
    ;; (send WORLD1 after-tick) -> WORLD1's state
    ;;                               (includes states of all the toys in WORLD1)
    ;;                                       after a tick
    ;; STRATEGY: Use of HOF map on ListOfToy in the world
    (define/public (after-tick)
      (make-world
       (map
        ;; Toy<%> -> Toy<%>
        ;; GIVEN: A Toy
        ;; RETURNS: A Toy as the given toy but after one tick
        (lambda (obj1)(send obj1 after-tick))obj)))

    ;; to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;;          painted on i
    ;; EXAMPLES: (send WORLD2 to-scene) -> SCENE2
    ;; DESIGN STRATEGY: Use HOF foldr on ListofToy<%> in the World
    (define/public (to-scene)
      (foldr
        ;; Toy<%> Scene ->  Scene 
        ;; GIVEN: A Toy and a Scene
        ;; RETURNS: A Scene as given with the given Toy painted on it
        (lambda (obj1 scene )(send obj1 add-to-scene scene))
          EMPTY-CANVAS obj))

    ;; after-key-event : KeyEvent -> Metatoy
    ;; GIVEN: a KeyEvent
    ;; RETURNS: A World like this one, but after the KeyEvent on it
    ;; EXAMPLES:
    ;;          (send WORLD2 after-key-event THROBBER-EVENT) -> 
    ;;                                  creates a new throbber in the world
    ;;          (send WORLD3 after-key-event Clock-EVENT) ->
    ;;                                  creates a new clock  in the world
    ;; STRATEGY: Cases on kev
    ;; "c", "t" and "p" create new Clock, new Throbber and new Politician respectively
    ;; other keystrokes are passed on to the objects in the Metatoy.
    (define/public (after-key-event ke)
        (cond
          [(key=? ke THROBBER-EVENT)
            (make-world
            (cons (make-throbber INITIAL-X INITIAL-Y)obj))]
           [(key=? ke CLOCK-EVENT)
             (make-world
            (cons (make-clock INITIAL-X INITIAL-Y)obj))]
           [(key=? ke POLITICIAN-EVENT)
             (make-world
            (cons (make-politician)obj))]
           [else
            (make-world
              (map
                (lambda (obj1)(send obj1 after-key-event ke))obj))]))


    ;; after-mouse-event : Integer Integer MouseEvent -> Metatoy
    ;; GIVEN: Two NonNegIntegers which represent the mouse position's x and y
    ;;        coordinates, and a MouseEvent mev
    ;; RETURNS: A Worldlike like given, but after the MouseEvent occurred
    ;; EXAMPLES:
    ;;          (send WORLD3 after-mouse-event 315 210 MOUSE-DOWN-EVENT)) ->
    ;;           
    ;;          selects the object(s) in the world inside which the mev occurred
    ;;
    ;;          (send WORLD7 after-mouse-event 315 210 MOUSE-DRAG-EVENT) ->
    ;;            
    ;;          drags the object(s) in the world inside which the mev occurred
    ;; STRATEGY: Cases on mev
    (define/public (after-mouse-event mx my me)
      (cond
        [(mouse=? me "button-down")(world-after-button-down mx my)]
        [(mouse=? me "button-up")(world-after-button-up mx my)]
        [(mouse=? me "drag")(world-after-drag mx my)]
        [(mouse=? me "move")(world-after-move mx my)]
        [else this]))

    ;;world-after-button-down : Integer Integer -> World
    ;;Given: Mouse coordinates after button-down event
    ;;Returns: A World after button-down event
    ;;Examples:
    ;; (send World1 world-after-button-down 315 210)->
    ;;  creates a world after performing the button-down event on the selected
    ;;   toy at given mouse co-ordinates
    ;;Strategy: Use HOFC map on the ListOfToy objs
    (define (world-after-button-down mx my)
      (make-world
        (map
         ;;GIVEN: Mouse coordinates
         ;;RETURNS: World after button down
         (lambda (obj1) (send obj1 after-button-down mx my))obj)))

    ;;world-after-button-up : Integer Integer -> World
    ;;Given: Mouse coordinates after button-up event
    ;;Returns: A World after button-up event
    ;;Examples:
    ;; (send World1 world-after-button-up 315 210)->
    ;;  creates a world after performing the button-up event on the selected
    ;;   toy at given mouse co-ordinates
    ;;Strategy: Use HOFC map on the ListOfToy objs
        (define (world-after-button-up mx my)
           (make-world
             (map
         ;;GIVEN: Mouse Coordinates
         ;;RETURNS: World after button up
         (lambda (obj1) (send obj1 after-button-up mx my))obj)))

    
    ;;world-after-drag : Integer Integer -> World
    ;;Given: Mouse coordinates after drag event
    ;;Returns: A World after drag event
    ;;Examples:
    ;; (send World1 world-after-button-drag 315 210)->
    ;;  creates a world after performing the button-drag event on the selected
    ;;   toy at given mouse co-ordinates
    ;;Strategy: Use HOFC map on the ListOfToy objs
        (define (world-after-drag mx my)
           (make-world
        (map
         ;;GIVEN: Mouse Coordinates
         ;;RETURNS: World after drag event
         (lambda (obj1) (send obj1 after-drag mx my))obj)))

    ;;world-after-move : Integer Integer -> World
    ;;Given: Mouse coordinates after move event
    ;;Returns: A World after move event
    ;;Examples:
    ;; (send World1 world-after-move 315 210)->
    ;;  creates a world after performing the move event on the selected
    ;;   toy at given mouse co-ordinates    
    ;;Strategy: Use HOFC map on the ListOfToy objs
     (define (world-after-move mx my)
           (make-world
        (map
         ;;GIVEN: mouse coordinates
         ;;RETURNS: World after move event of the mouse
         (lambda (obj1) (send obj1 after-move mx my))obj)))
    ))
;; Constructor template for World%: 
;; (new World% [objs ListOfToy])
;; Interpretation: An object of class World% takes signals from
;; big-bang and distributes them to its objects as appropriate.
(define (make-world obj )
  (new World% [obj obj]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-metatoy : ListOfToys -> Metatoy
;; RETURNS: a Metatoy with the given list of toys.
;; NOTE: The Metatoy<%> interface extends the World<%> interface, so the
;; result of make-metatoy is something that big-bang can use as a world.

(define (make-metatoy listtoys)
  (make-world listtoys INITIAL-STATE ))

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
    

    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing clock location's x-coordinate value
    ;; EXAMPLES: (send Clock1 toy-x) -> 300
     (define/public (toy-x) x)
    
    
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing clock location's y-coordinate value
    ;; EXAMPLES: (send Clock1 toy-y) -> 250
     (define/public (toy-y) y)
    
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Integer representing clock location's second count value
    ;; EXAMPLES: (send Clock1 toy-data) -> 0
     (define/public (toy-data) second-count)
    
    ;; after-key-event :  KeyEvent -> Clock
    ;; GIVEN: a KeyEvent kev
    ;; RETURNS: A Worldlike like given, but after the KeyEvent occurred
    ;; EXAMPLES:
    ;;          (send clock1 after-key-event  "c")) -> clock1
    ;;           no changes
     (define/public (after-key-event kev) this)

    ; after-button-down : Integer Integer -> Clock
    ; GIVEN: the coordinates of a button-down event
    ; RETURNS: A clock after performing the button down event
    ; Examples:
    ; (send Clock1 after-button-down 315 210)->
    ;  creates a Clock after performing the button-down event on the selected
    ;;   Clock at given mouse co-ordinates
    ; STRATEGY: Cases on whether the event is in the clock.
    ; If the mouse event occurs withing the clock, then selected? becomes true.
    (define/public (after-button-down mx my)
       (if (check-in-clock? mx my)
           (new Clock%
               [x x]
               [y y]
               [second-count  second-count]
               [selected? true]
               [curr-mx (- mx x)]
               [curr-my (- my y)])
       this))
    
    ; check-in-clock? : Integer Integer -> Boolean
    ; GIVEN: the coordinates of a mouse pointer
    ; Returns; true iff the mouse pointer is within the clock
    ; EXAMPLES:
    ; (check-in-clock? CLOCK1 301 251) -> true
    ; STRATEGY: combine simpler functions
     (define (check-in-clock? x1 y1)
       (and(<= (abs (- x1 x)) CLOCK-WIDTH)
           (<= (abs (- y1 y)) CLOCK-HEIGHT)))


    ; after-button-up : Integer Integer -> Toy
    ; GIVEN: the location of a button-up event
    ; RETURNS: A clock after performing the button up event
    ; STRATEGY: Cases on whether the event is in the clock.
    ; EXAMPLES:
    ; (send Clock1 after-button-up 315 210)->
    ;  creates a clock after performing the button-up event on the selected
    ;   clock at given mouse co-ordinates
    ; If the clock is selected, then unselect it.
     (define/public (after-button-up mx my)
          (new Clock%
               [x x]
               [y y]
               [second-count  second-count]
               [selected? false]
               [curr-mx  curr-mx]
               [curr-my  curr-my]))

    ; after-drag : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; Returns: A clock after drag event
    ; Examples:
    ; (send Clock after-drag 315 210)->
    ;  creates a clock after performing the button-drag event on the selected
    ;   clock at given mouse co-ordinates 
    ; STRATEGY: Cases on whether the event is in the clock.
    ; If the clock is selected, then drag it.
    (define/public (after-drag mx my)
            (if selected?
           (new Clock%
               [x (- mx curr-mx)]
               [y (- my curr-my)]
               [second-count  second-count]
               [selected? true]
               [curr-mx curr-mx]
               [curr-my curr-my])
       this))
    
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: A Scene
    ;; RETURNS: a Scene like the given one, but with this Clock painted
    ;;          on it
    ;; EXAMPLES: (send CLOCK1 add-to-scene EMPTY-CANVAS) -> CLOCK-SCENE
    ;; DESIGN STRATEGY: Combine simpler functions.
     (define/public (add-to-scene s)
       (place-image CLOCK-PIC x y s))

    ;; after-tick : -> Toy
    ;; RETURNS: A clock like this one, but how it should be after a tick
    ;; DETAILS: the world timer increases by 1 at each tick.
    ;; EXAMPLES:
    ;; (send clock1 after-tick) -> Clock1 state after 1 tick the timer counter
    ;;                              incraesed by 1
    ;;                              
     (define/public (after-tick)
            (new Clock%
               [x x]
               [y y]
               [second-count (+ 1 second-count)]
               [selected? selected?]
               [curr-mx curr-mx]
               [curr-my curr-my]))


    ;; after-move: Integer Integer -> Clock      
    ;;Given: Mouse coordinates
    ;;Returns: A Clock 
    ;;Examples:
    ;; (send Clock1 after-move 315 210)-> creates same Clock1    
         (define/public (after-move mx my)
        this)
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CLOCK1 (make-clock 300 250))
(define CLOCK2 (send CLOCK1 after-button-up 20 20))
(define CLOCK3 (send CLOCK2 after-button-down 320 240))
(define CLOCK4 (send CLOCK3 after-tick))
(define CLOCK5 (send CLOCK3 after-drag 20 20))
(define CLOCK6 (send CLOCK1 after-key-event "t"))
(define CLOCK7 (send CLOCK1 after-button-down 180 190))
(define CLOCK8 (send CLOCK1 after-button-up 580 890))
(define CLOCK9 (send CLOCK1 after-move 580 890))
(define CLOCK10 (send CLOCK2 after-drag 20 20))
(define SCENE (place-image (overlay
                            (text (format "~a" 0)
                                  CLOCK-FONT
                                  CLOCK-FONT-COLOR)
                                  (rectangle  CLOCK-WIDTH CLOCK-HEIGHT FILL CLOCK-COLOR))
                                  300 250 EMPTY-CANVAS))
(define CLOCK11 (send CLOCK3 add-to-scene EMPTY-CANVAS))
(define CLOCK12 (new Clock% [x 200][y 150][second-count 0]))

(begin-for-test
    (check-equal?
    (send CLOCK1 toy-x) 300 "the x co-ordinate of the toy extracted")

    (check-equal?
    (send CLOCK1 toy-y) 250 "the y co-ordinate of the toy extracted")

    (check-equal?
    (send CLOCK12 toy-data) 0 "the radius of the toy extracted")



    )
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
     (field [COLOR "green"])
     (field [THROBBER-PIC (circle radius fill COLOR)])

     (super-new)

      ;; -> Integer
      ;; GIVEN: no arguments
      ;; RETURNS: Integer representing throbber location's x-coordinate value
      ;; EXAMPLES: (send THROB1 toy-x) -> 300
     (define/public (toy-x) x)
     
      ;; -> Integer
      ;; GIVEN: no arguments
      ;; RETURNS: Integer representing throbber location's y-coordinate value
      ;; EXAMPLES: (send THROB1 toy-y) -> 250
     (define/public (toy-y) y)
     
      ;; -> Integer
      ;; GIVEN: no arguments
      ;; RETURNS: Integer representing throbber location's y-coordinate value
      ;; EXAMPLES: (send THROB1 toy-data) -> 5
     (define/public (toy-data) radius)


    ;; after-key-event :  KeyEvent -> Throbber
    ;; GIVEN: a KeyEvent mev
    ;; RETURNS: A Worldlike like given, but after the KeyEvent occurred
    ;; EXAMPLES:
    ;;          (send THROB1 after-key-event  "c")) -> THROB1
    ;;           no changes  
     (define/public (after-key-event kev) this)

    ; after-button-down : Integer Integer -> Throbber
    ; GIVEN: the coordinates of a button-down event
    ; RETURNS: A Throbber after performing the button down event
    ; Examples:
    ; (send THROB1 after-button-down 315 210)->
    ;  creates a Throbber after performing the button-down event on the selected
    ;;   Throbber at given mouse co-ordinates
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the mouse event occurs withing the throbber, then selected? becomes true.
     (define/public (after-button-down mx my)
       (if (check-in-throbber? mx my)
           (new Throbber%
                [x x][y y]
                [radius radius]
                [selected? true]
                [increasing increasing]
                [curr-mx curr-mx]
                [curr-my curr-my]
                [fill OUTLINE])
                this))

    ; check-in-throbber? : Integer Integer -> Boolean
    ; GIVEN: the coordinates of a mouse pointer
    ; Returns; true iff the mouse pointer is within the throbber
    ; EXAMPLES:
    ; (check-in-throbber? THROB1 301 251) -> true
    ; STRATEGY: combine simpler functions

     (define (check-in-throbber? x1 y1)
       (<= (+ (sqr (- x x1))(sqr (- y y1))) (sqr radius)))

     ; after-button-up : Integer Integer -> Throbber
     ; GIVEN: the location of a button-up event
     ; RETURNS: A Throbber after performing the button down event
     ; EXAMPLES:
     ; (send THROB1 after-button-up 315 210)->
     ;  creates a Throbber after performing the button-up event on the selected
     ;   Throbber at given mouse co-ordinates
     ; STRATEGY: Cases on whether the event is in the throbber.
     ; If the throbber is selected, then unselect it.
     (define/public (after-button-up mx my)
           (new Throbber%
                [x x][y y]
                [radius radius]
                [selected? false]
                [increasing increasing]
                [curr-mx curr-mx]
                [curr-my curr-my]
                [fill SOLID]))

     ; after-drag : Integer Integer -> Throbber
     ; GIVEN: the location of a drag event
     ; Returns: A Throbber after drag event
     ; Examples:
     ; (send THROB1 after-drag 315 210)->
     ;  creates a Throbber after performing the after-drag event on the selected
     ;   Throbber at given mouse co-ordinates 
     ; STRATEGY: Cases on whether the event is in the throbber.
     ; If the throbber is selected, then drag it.
     (define/public (after-drag mx my)
       (if selected?
           (new Throbber%
                [x (- mx curr-mx)]
                [y (- my curr-my)]
                [radius radius]
                [selected? true]
                [increasing increasing]
                [curr-mx curr-mx]
                [curr-my curr-my]
                [fill fill])
                this))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: A Scene
    ;; RETURNS: a Scene like the given one, but with this throbber painted
    ;;          on it
    ;; EXAMPLES: (send THROB1 add-to-scene EMPTY-CANVAS) -> THROBBER-SCENE
    ;; DESIGN STRATEGY: Combine simpler functions.
    
     (define/public (add-to-scene s)
       (place-image THROBBER-PIC x y s))

     ;; after-tick : -> Throbber
     ;; RETURNS: A throbber like this one, but how it should be after a tick
     ;; EXAMPLES:
     ;; (send THROB1 after-tick) -> THROB1 state after 1 tick the timer counter
     ;;                             with radius increased by 1 if the current
     ;;                             radius is <=20 else radius decreased by 1
     ;;                             if the current radius >=5
     ;; STRATEGY: Cases on the value of radius
     ;; DETAILS: the throbber radius increases by 1 at each tick till the radius is 20 after which
     ;;decreases by 1 till it reached radius 5.
     (define/public (after-tick)
       (cond
         [(or (and (< radius 20) increasing)
               (< radius 5))
            (new Throbber%
                [x x][y y]
                [radius  (+ radius 1)]
                [selected? selected?]
                [increasing true]
                [curr-mx curr-mx]
                [curr-my curr-my]
                [fill fill])]
           [(new Throbber%
                [x x][y y]
                [radius  (- radius 1)]
                [selected? selected?]
                [increasing false]
                [curr-mx curr-mx]
                [curr-my curr-my]
                [fill fill])]))

    ;; after-move: Integer Integer -> Throbber    
    ;;Given: Mouse coordinates after move event
    ;;Returns: A Clock after move event
    ;;Examples:
    ;; (send THROB1 after-move 315 210)->  creates same THROB1 
      (define/public (after-move mx my)
        this)
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      ;; RETURNS: Integer representing throbber location's y-coordinate value
      ;; EXAMPLES: (send POLITICIAN toy-y) -> 250
    (define/public (toy-x) x)
     (define/public (toy-y) y)
    ;;Distance between the politician and the mouse point after mouse move event
     (define/public (toy-data)(round(calculate-hypo x y poli-mx poli-my)))
    ;;___________________________________________________
    ; after-move : Integer Integer -> Toy
    ; GIVEN: the location of the mouse coordinates after mouse move event
    ;;RETURNS: records the new mouse points inside the politician
    (define/public (after-move mx my)
        (new Politician%
             [x x]
             [y y]
             [poli-mx mx]
             [poli-my my]
             [POLITICIAN-IMG POLITICIAN-IMG]))
    ;;___________________________________________________
    
    ;;calculate-hypo: Integer,Integer,Integer,Integer
    ;;GIVEN: x and y coordinates of politician and mouse
    ;;x, y: Politician center coordinates
    ;;mx,my: Mouse coordinates
    ;;RETURNS: distance between Politician's centre to Mouse coordinates
    (define (calculate-hypo x y mx my)
      (sqrt(+ (sqr(- mx x)) (sqr(- my y)))))
    ;;___________________________________________________
    ;;new-x:Integer,Integer,Integer,Integer-> Integer
    ;;GIVEN: x and y coordinates of politician and mouse
    ;;x, y: Politician center coordinates
    ;;mx,my: Mouse coordinates
    ;;RETURNS: New x coordinates of the Politician on the line
    (define (new-x x y mx my)
      (/ (- mx x) (calculate-hypo x y mx my)))
    ;;___________________________________________________
    ;;new-y:Integer,Integer,Integer,Integer
    ;;GIVEN: x and y coordinates of politician and mouse
    ;;x, y: Politician center coordinates
    ;;mx,my: Mouse coordinates
    ;;RETURNS: New y coordinates of the Politician on the line
    (define(new-y x y mx my)
      (/ (- my y) (calculate-hypo x y mx my)))
    ;;___________________________________________________
    ;; after-tick : -> Toy
    ;; RETURNS: A Politician like this one, but how it should be after a tick
    ;; DETAILS: the Politician comes one step closer to the mouse at each tick.
    ;;But repels from the mouse point if within radius of 75 pixels of the mouse. Repeats the process
    (define/public (after-tick)
      (if (>= (calculate-hypo x y poli-mx poli-my) 75)
          (new Politician%
                  [x (+ x (* 10(new-x x y poli-mx poli-my)))]
                  [y (+ y (* 10(new-y x y poli-mx poli-my)))]
                  [poli-mx poli-mx]
                  [poli-my poli-my]
                  [POLITICIAN-IMG POLITICIAN-IMG])      
             (new Politician%
                  [x (- x (* 50(new-x x y poli-mx poli-my))) ]
                  [y (- y (* 50(new-y x y poli-mx poli-my)))]
                  [poli-mx poli-mx]
                  [poli-my poli-my]
                  [POLITICIAN-IMG
                   (if (equal? POLITICIAN-IMG hillary-image) trump-image hillary-image)])))
    ;;___________________________________________________
    ;No effect on the Polotician on occurance of key event
    (define/public (after-key-event kev)
        this)      
    ;;___________________________________________________
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this Politician painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image POLITICIAN-IMG x y scene))
      ;;___________________________________________________
    ;;No effect on the Politician on occurance of the following mouse event  
      (define/public (after-button-down mx my)
        (new Politician%
             [x x]
             [y y]
             [poli-mx mx]
             [poli-my my]
             [POLITICIAN-IMG POLITICIAN-IMG]))
      (define/public (after-drag mx my)
        (new Politician%
             [x x]
             [y y]
             [poli-mx mx]
             [poli-my my]
             [POLITICIAN-IMG POLITICIAN-IMG]))
      (define/public (after-button-up mx my)
        (new Politician%
             [x x]
             [y y]
             [poli-mx mx]
             [poli-my my]
             [POLITICIAN-IMG POLITICIAN-IMG]))
    
    ;; function to test Politician class output
    ;; for-test:politiciantest-state : -> ListOfStateVariable
    ;; GIVEN : no argument
    ;; RETURNS : return all the variable of the given state
    (define/public (for-test:politiciantest-state)
      (list x y poli-mx poli-my hillary-image))

    ))
 
(begin-for-test
  (local
    ((define INIT-POLI (make-politician))
     (define INIT-POLI-DEFAULT (new Politician% [x INITIAL-X]
                                                         [y INITIAL-Y]
                                                         [poli-mx 0]
                                                         [poli-my 0]
                                                         [POLITICIAN-IMG hillary-image]))
     (define INIT-POLI-AFTER-TICK (send INIT-POLI after-tick))
     (define INIT-POLI-AFTER-KEY (send INIT-POLI after-key-event "p"))
     (define INIT-POLI-AFTER-DOWN
       (send INIT-POLI after-button-down INITIAL-X INITIAL-Y))
     (define INIT-POLI-AFTER-UP (send INIT-POLI after-button-up 20 30))
     (define INIT-POLI-DEF-AFTER-TICK
       (send INIT-POLI-DEFAULT after-tick))
     (define INIT-POLI-AFTER-MOVE
       (send INIT-POLI-AFTER-DOWN after-move 20 30))
     (define INIT-POLI-AFTER-MOVE-75
       (send INIT-POLI-AFTER-DOWN after-move 230 300))
     (define INIT-POLI-AFTER-DRAG-DOWN
       (send INIT-POLI-AFTER-DOWN after-drag 20 30))
     (define INIT-POLI-AFTER-DRAG (send INIT-POLI after-drag 20 30))
     (define INIT-POLI-AFTER-TICK-75
       (send INIT-POLI-AFTER-MOVE-75 after-tick))
     (define INIT-POLI-75-AFTER-TICK
       (send INIT-POLI-AFTER-TICK-75 after-tick))
     (define init-repultion
       (send (new Politician% [x INITIAL-X][y INITIAL-Y][poli-mx 240]
                  [poli-my 320] [POLITICIAN-IMG hillary-image]) after-tick))
     (define init-repultion1
       (send (new Politician% [x INITIAL-X][y INITIAL-Y][poli-mx 240]
                  [poli-my 320] [POLITICIAN-IMG hillary-image]) after-tick)))
    
     (check-equal?
      (send INIT-POLI toy-x)
      INITIAL-X)
    (check-equal?
      (send INIT-POLI toy-y)
      INITIAL-Y)
    (check-equal?
      (send INIT-POLI toy-data)
      300)
    (check-equal?
     (send INIT-POLI-AFTER-TICK toy-data)
     290)
    (check-equal?
     (send INIT-POLI-AFTER-KEY for-test:politiciantest-state)
     (list INITIAL-X INITIAL-Y 250 0 hillary-image))
    (check-equal?
     (send INIT-POLI-AFTER-DOWN for-test:politiciantest-state)
     (list INITIAL-X INITIAL-Y INITIAL-X INITIAL-Y hillary-image))
    (check-equal?
     (send INIT-POLI-AFTER-UP for-test:politiciantest-state)
     (list INITIAL-X INITIAL-Y 20 30 hillary-image))
    (check-equal?
     (send INIT-POLI-DEF-AFTER-TICK for-test:politiciantest-state)
     (list 243.5981560033552 292.3177872040262 0 0 hillary-image))
    (check-equal?
     (send INIT-POLI-AFTER-MOVE for-test:politiciantest-state)
     (list INITIAL-X INITIAL-Y 20 30 hillary-image))
    (check-equal?
     (send INIT-POLI-AFTER-MOVE-75 for-test:politiciantest-state)
     (list INITIAL-X INITIAL-Y 230 300 hillary-image))
    (check-equal?
     (send INIT-POLI add-to-scene EMPTY-CANVAS)
     (place-image hillary-image INITIAL-X INITIAL-Y EMPTY-CANVAS))
    (check-equal?
     (send INIT-POLI-DEFAULT add-to-scene EMPTY-CANVAS)
     (place-image hillary-image INITIAL-X INITIAL-Y EMPTY-CANVAS))
    (check-equal?
     (send INIT-POLI-AFTER-TICK-75 for-test:politiciantest-state)
     (list 300 300 230 300 hillary-image))
    (check-equal?
     (send INIT-POLI-75-AFTER-TICK for-test:politiciantest-state)
     (list 350 300 230 300 hillary-image))
    (check-equal?
     (send init-repultion for-test:politiciantest-state)
     (list 272.3606797749979 255.27864045000422 240 320 hillary-image))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(define THROB1 (make-throbber 300 250))
(define THROB2 (send THROB1 after-tick))
(define THROB3 (send THROB2 after-button-down 302 250))
(define THROB4 (send THROB3 after-drag 20 20))
(define THROB5 (send THROB4 after-button-up 20 20))
(define THROB6 (send THROB1 add-to-scene EMPTY-CANVAS ))
(define THROB-SCENE (place-image (circle 5 "solid" "green")
                                 300 250 EMPTY-CANVAS))
(define THROB7 (send THROB3 after-tick))
(define THROB8 (send THROB1 after-button-down 10 10))
(define THROB9 (send THROB8 after-drag 10 10))
(define THROB10 (send THROB9 after-button-up 10 10))
(define THROB11 (new Throbber% [x 300][y 250][radius 20][increasing 1]))
(define THROB12 (send THROB11 after-tick))
(define THROB13 (send THROB12 after-tick))


(begin-for-test
    (check-equal?
    (send THROB1 toy-x) 300 "the x co-ordinate of the toy extracted")

    (check-equal?
    (send THROB1 toy-y) 250 "the y co-ordinate of the toy extracted")

    (check-equal?
    (send THROB1 toy-data) 5 "the radius of the toy extracted")

    (check-equal?
    (send THROB1 after-key-event "x") THROB1
     "the radius of the toy extracted")

    (check-equal? THROB6 THROB-SCENE
     "returns scene with the Throbber painted on it")

      (check-equal?
    (send THROB1 after-move 350 250) THROB1
     "the radius of the toy extracted")

      )
  
 (define WORLD1 (make-world 2 0))
(define WORLD2  (make-world
                            (list (make-clock 300 250)
                                  (make-throbber 300 250)
                                  (make-politician)) 0))
(define WORLD3 (make-world empty 0))
(define WORLD4 (make-world empty 120))
(define WORLD5 (send WORLD2 after-key-event THROBBER-EVENT)) 
(define WORLD6 (send WORLD1 after-key-event CLOCK-EVENT))
(define WORLD7 (send WORLD6 after-key-event "t"))
(define WORLD8 (send WORLD6 after-key-event "c"))
(define WORLD16 (send WORLD2 after-key-event "x"))
(define WORLD111 (send WORLD2 after-key-event "p"))
(define WORLD9 (send WORLD2 after-mouse-event 310 250 "button-down"))
(define WORLD10 (send WORLD2 after-mouse-event 310 250 "button-up"))
(define WORLD11 (send WORLD2 after-mouse-event 310 250 "drag"))
(define WORLD12 (send WORLD2 after-mouse-event 310 250 "move"))
(define WORLD13 (send WORLD2 after-mouse-event 310 250 "enter"))
(define WORLD14 (send WORLD2 to-scene))
(define WORLD15 (send WORLD2 after-tick))
(define WORLD17 (send WORLD2 get-toys))

(define METATOY1 (make-metatoy (list (make-clock 300 250)
                                  (make-throbber 300 250))))

(define WORLD31 (initial-world))

