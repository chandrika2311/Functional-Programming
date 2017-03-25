#lang racket

;; displays the controller based on input with the handle with a stagnant
;; particle or velocity details or position details

(require rackunit)
(require "Interfaces.rkt")
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)

;; CONSTANTS
(define INITIAL-STATE 0)
(define RECTANGLE-TYPE "outline")
(define RECTANGLE-TYPE-SOLID "solid")
(define INNER-REC "blue")
(define OUTER-REC "black")
(define RECTANGLE-COLOR-SOLID "white")
(define DOT-RADIUS 2)
(define DOT-COLOR "black")
(define RADIUS 10)
(define CIRCLE-COLOR "red")
(define CIRCLE-TYPE "solid")
(define TEXT-SIZE 12)
(define SMALL-TEXT-SIZE 10)
(define UNSELECTED "black")
(define SELECTED "red")

(define DRAG-COLOR "red")
(define UNDRAG-COLOR "black")
(define HANDLE 10)
(define HANDLE-TYPE "outline")

(provide DragContainer%
         RECTANGLE-TYPE
         RECTANGLE-TYPE-SOLID
         INNER-REC
         OUTER-REC
         RECTANGLE-COLOR-SOLID
         DOT-RADIUS
         DOT-COLOR
         RADIUS
         CIRCLE-TYPE
         CIRCLE-COLOR
         TEXT-SIZE
         SMALL-TEXT-SIZE
         UNSELECTED
         SELECTED)

;; Every controller class is a subclass of ControllerContainer%
;; a DragContainer is a (new DragContainer% [model Model<%>]
;;                                          [x Integer]  [y Integer])

                                                    
(define DragContainer%
  (class* object% (Controller<%>)


     ;; model that will be related to the controller
    (init-field model);; Model<%>
    
    ;; the position of the center of the controller
    (init-field [x 300][y 250]) ;; Integer

 
    
    ;; the detials of the particle
    (field [particle-x INITIAL-STATE]) ;; Real
    (field [particle-y INITIAL-STATE]) ;; Real
    (field [particle-vx INITIAL-STATE]) ;; Integer
    (field [particle-vy INITIAL-STATE]) ;; Integer
    
    ;; if selected? is true then communnication with the  controller
    ;; via keyevents and mouse events is possible
    ;; drag? signifies if the particle is draggable
    ;;  if it's selected, the differences
    ;; between the coodidates of mouse
    ;; position and particle-x particle y; else any value
    (field [selected? false]) ; Boolean
    (field [drag? false]) ;; Boolean
    (field [saved-mx INITIAL-STATE]) ;; Integer
    (field [saved-my INITIAL-STATE]) ;; Integer
    
    (super-new)

        
    ;; send this controller to the repsective mmodel's ListOfControllers
    (send model register this)
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETUNS: a scene like the given one but with the object painted on it
    ;; STRATEY: place the image at given location
    (define/public (add-to-scene scene)
      (place-image (display-image) x y scene))
    
    ;; dis-image: -> Image
    (define (display-image)
      (let ((the-details-image (details-image)))
      (overlay/align "left" "top"
                     (square HANDLE HANDLE-TYPE (color-choice))
                     (overlay the-details-image
                              (bgrnd-image the-details-image)))))
    
    ;; color-choice: -> String
	;; GIVEN: no arguments
	;; RETURNS: if the drag? is true then "red" else "black"
	;; STRATEGY: Cases on drag?
    (define (color-choice)
      (if drag? DRAG-COLOR UNDRAG-COLOR))
    
    ;; abstract method to update the image 
    (abstract details-image)

     ;; abstract method to update the image 
    (abstract bgrnd-image)

    
    ;; abstract method to update the model after button down
    (abstract model-after-button-down)
    
    ;; abstract method to update the model after button up
    (abstract model-after-button-up)
    
    ;; abstract method to update the model after drag
    (abstract model-after-drag)
    
    ;; recieve-signal: Signal -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: figure out the signal and update data
    ;; STRATEGY: Cases on Signal s
    (define/public (recieve-signal s)
      (cond
        [(report-position? s)
         (set! particle-x (report-position-pos-x s))
         (set! particle-y (report-position-pos-y s))]
        [(report-velocity? s)
         (set! particle-vx (report-velocity-vx s))
         (set! particle-vy (report-velocity-vy s))]))
    
    ;; after-key-event: KeyEvent -> Void
    ;; EFFECT: update the controller according to given keyevent
    (abstract after-key-event)
    
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: no change in the draggable object
    (define/public (after-tick)
      'scene-after-tick)
    
    ;;after-button-down: Integer Integer -> Void
    ;; GIVEN: the coordinates for the mouse button down event
    ;; EFFECT: if the location of button down is inside the handle
    ;;         then the view gets selected and updates the model object
    ;; STRATEGY: Cases on if the event is inside the handle
    (define/public (after-button-down mx my)
      (cond
        [(inside-handle? mx my)
         (begin
           (set! drag? true)
           (set! saved-mx (- mx x))
           (set! saved-my (- my y)))]
        [(in-object? mx my)
         (begin
           (set! selected? true)
           (model-after-button-down mx my))]
        [else "nothing"]))
    
      ;; inside-handle?: Real Real -> Boolean
    ;; GIVEN: a location
    ;; RETURNS: true iff the given location is within this object's handle
    ;; DESIGN STRATEGY: Combine simpler functions
    (define (inside-handle? x1 y1)
      (let ((wid (/ (image-width (display-image)) 2))
            (hei (/ (image-height (display-image)) 2)))
        (and (<= (- x wid) x1 (+ (- x wid) RADIUS))
             (<= (- y hei) y1 (+ (- y hei) RADIUS)))))
    
    ;; in-object?: Real Real -> Boolean
    ;; GIVEN: a location
    ;; RETURNS: true iff the location is withing the bounds of this object
    ;; DESIGN STRATEGY: Combine simpler functions
    (define (in-object? x1 y1)
      (let ((wid (/ (image-width (display-image)) 2))
            (hei (/ (image-height (display-image)) 2)))
        (and (<= (- x wid) x1 (+  x wid))
             (<= (- y hei) y1 (+  y hei)))))
    
    
    ;;after-button-up: Integer Integer -> Void
    ;; GIVEN: the coordinates for the mouse button up event
    ;; EFFECT: changes selected and drag flag to false
    (define/public (after-button-up mx my)
      (begin
        (set! selected? false)
        (set! drag? false)
        (model-after-button-up mx my)))
    
    ;;after-drag: Integer Integer -> Void
    ;; GIVEN: the coordinates for the mouse drag event
    ;; EFFECT: if drag? true then move the particle to the relative mouse
    ;;         position and current position else if selected? update
    ;;         model for respective object
    ;; STRATEGY: Cases on drag? and selected?
    (define/public (after-drag mx my)
      (cond
        [selected?
         (model-after-drag mx my)]
        [drag?
         (set! x (- mx saved-mx))
         (set! y (- my saved-my))]
        [else "nothing"]))

      ;; after-move: Integer Integer  -> Void
    ;; EFFECT: update the controller according to given after move
    (define/public (after-move mx my) 'no-change-after-move)
    
    ;; -> Real
    ;; Returns the particle position's x coordinate value
    (define/public (for-test:get-particle-x)
      particle-x)
    
    ;; -> Real
    ;; Returns the particle position's y coordinate value
    (define/public (for-test:get-particle-y)
      particle-y)
    
    ;; -> Integer
    ;; Returns the particle's velocity vx
    (define/public (for-test:get-particle-vx)
      particle-vx)
    
    ;; -> Integer
    ;; Returns the particle's velocity vy
    (define/public (for-test:get-particle-vy)
      particle-vy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DragContainer% contains abstract methods. So it cannot be instantiated in order
;; to be tested.