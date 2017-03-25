#lang racket


;; displays as an outline rectangle with the particle moving in the x-plane
;; the rectangle is draggable after a mouse click inside its handle

(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "DragContainer.rkt")
(require "Model.rkt")

(provide XController%)

;; An X controller, which shows a representation of the particle bouncing
;; in the rectangle. With this controller, the user can drag the
;; particle using the mouse. Dragging the mouse causes the particle
;; to follow the mouse pointer via a Smooth Drag.

;; a XController% is a (new XConatiner% [model Model<%>]
;;                                       [x Integer][y Integer])
;; x, y are optional

(define XController%
  (class* DragContainer% (Controller<%> HandleController<%>)
    
    ;; mopdel to whcih the position controller is connected
    (inherit-field model); (Model<%>)
    
    ;; position of the particle
    (inherit-field particle-x particle-y ) ;; RealNum RealNum
    
    ;; if it is selected, then position of last button-down
    (inherit-field saved-mx saved-my) ;;  Integer Integer 
    
    ;; size of XY Controller
    (field [width 150][height 40]) ;; units in pixels
    
    (super-new)
    
    ;; after-key-event: kev -> void
    ;; GIVEN: a KeyEvent kev
    ;; EFFECT: no change in xy controller on any key event
    (define/override (after-key-event ke) "nothing")
    
    
    ;; model-after-button-down: Integer Integer -> void
    ;; GIVEN: a location
    ;; EFFECT: pauses the model, and modifies the saved-mx value
    (define/override (model-after-button-down mx my)
      (begin
      (send model execute-command
            (make-set-pause true))
      (set! saved-mx (- mx particle-x))))
    
    ;; model-after-button-up: Integer Integer -> void
    ;; GIVEN: a location
    ;; EFFECT: unpauses the model, 
    (define/override (model-after-button-up mx my)
      (send model execute-command
            (make-set-pause false)))
    
    ;; model-after-drag: Integer Integer -> void
    ;; GIVEN: a location
    ;; EFFECT: update the model with the new location of the particle
    (define/override (model-after-drag mx my)
      (begin
        (send model execute-command
              (make-set-position
               (- mx saved-mx) particle-y))))
    
    ;; details-image:  -> Image
    ;; GIVEN:no arguments
    ;; RETURNS: the image that shows the position and the velocities
    ;;          of the particle
    (define/override (details-image)
      (overlay
       (rectangle width height RECTANGLE-TYPE INNER-REC)
       (place-image
        (overlay
         (circle  DOT-RADIUS CIRCLE-TYPE DOT-COLOR)
         (circle  RADIUS CIRCLE-TYPE CIRCLE-COLOR))
        particle-x (/ height 2)
        (rectangle width height RECTANGLE-TYPE-SOLID
                   RECTANGLE-COLOR-SOLID))))
    
    ;; bgrnd-image: Image -> Image
    ;; GIVEN: image
    ;; RETURNS: rectangle in background with updated size
    ;; STRATEGY: Combine simpler function
    (define/override (bgrnd-image the-details-image)
      (rectangle
       (max width (+ (image-width the-details-image) 50))
       (image-height the-details-image)
       RECTANGLE-TYPE OUTER-REC))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING:

(begin-for-test
 (local 
  ((define model (new Model%))
   (define cx (new XController% [model model])))
   (send cx after-key-event "left")
   (send cx model-after-button-up 20 30)
   (send cx model-after-button-down 50 60)
   (send cx model-after-drag 55 66)
   (send cx model-after-button-up 55 66)
   (send cx details-image)
   (send cx bgrnd-image (empty-scene 150 150))
   ))
 