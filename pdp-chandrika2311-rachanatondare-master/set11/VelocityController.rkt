#lang racket

(require rackunit)
(require plot/utils)
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "DragContainer.rkt")
(require "extras.rkt")
(require "Model.rkt")


(provide VelocityController%)

;; A velocity controller, handles and manipulates the velocity of the
;; particle using the arrow keys to move the particle in the x or
;; y direction.

;; a VelocityController is a (new VelocityController% [model Model<%>]
;;                                                     [x Integer][y Integer])

;; x, y are optional

(define VelocityController%
  (class* DragContainer% (Controller<%> HandleController<%>)
    
    ;;A model connected to position controller
    (inherit-field model) ;;Model<%>
    
    ;; selected by mouse or not 
    ;; if it is selected, then position of last button-down 
    (inherit-field selected? saved-mx saved-my);; Boolean Integer Integer
    
    ;;Position with repect to x an dy co-ordinates of the particle
    (inherit-field particle-x particle-y particle-vx particle-vy)
    ;; RealNum RealNum Integer Integer   
    
    ;; size of the controller
    (field [width 120][height 50]) ;; units in pixels
    
    (super-new)
    
    ;; after-key-event: ke -> void
    ;; GIVEN: a KeyEvent ke
    ;; EFFECT: if the controller is selected, sends velocity adjust value
    ;;        to the model according to given keyevent    
    ;; EXAMPLES: (send p1 after-key-event "right") -> send the manipulated values
    ;;            5 0 to model
    ;; STRATEGY: cases on selected? and ke
    (define/override (after-key-event ke)
      (if selected?
          (cond
            [(key=? ke "up") (manage-velocity 0 -5)]
            [(key=? ke "down") (manage-velocity 0 5)]
            [(key=? ke "left") (manage-velocity -5 0)]
            [(key=? ke "right") (manage-velocity 5 0)])
          4567))
    
    
    ;; manage-velocity: Integer Integer: -> Void
    ;; GIVEN: the values to be used 
    ;; RETURNS: updated values of the partticle and updating the respective
    ;;          model
    ;; STRATEGY: Combine simpler functions
    (define (manage-velocity dvx1 dvy1)
      (send model execute-command
            (make-increment-velocity dvx1 dvy1)))
    
    ;; details-image:  -> Image
    ;; GIVEN:no arguments
    ;; RETURNS: the image that shows the position and the velocities
    ;;          of the particle
    (define/override (details-image)
      (above
       (text "Arrow keys change velocity" SMALL-TEXT-SIZE (text-color))
         (text (string-append
                 "X = " (real->decimal-string* particle-x 1 2)
                 " Y = " (real->decimal-string* particle-y 1 2))
               TEXT-SIZE
               (text-color))
          (text (string-append
                 "VX = " (real->decimal-string* particle-vx 1 1)
                 " VY = " (real->decimal-string* particle-vy 1 1))
               TEXT-SIZE
               (text-color))))


    ;; text-color: -> String
    ;; GIVEN: no arguments
    ;; RETUNRS: color of the text based on selected?
    ;; STRATEGY: Cases on selected?
    (define (text-color)
      (if selected? SELECTED UNSELECTED))

    ;; bgrnd-image: Image -> Image
    ;; GIVEN: image
    ;; RETURNS: rectangle in background with updated size
    ;; STRATEGY: Combine simpler function
   (define/override (bgrnd-image the-details-image)
      (rectangle
       (max width (+ (image-width the-details-image)50))
       (max height (+ (image-height the-details-image)10))
       RECTANGLE-TYPE OUTER-REC))
    
    ;; model-after-button-down: Integer Integer -> void
    ;; GIVEN: a location
    ;; EFFECT: no change in model
    (define/override (model-after-button-down mx my)
      908)
    
    ;; model-after-button-up: Integer Integer -> void
    ;; GIVEN: a location
    ;; EFFECT: no change in model
    (define/override (model-after-button-up mx my)
      1908)
    
    ;; model-after-drag: Integer Integer -> void
    ;; GIVEN: a location
    ;; EFFECT: no change in model
    (define/override (model-after-drag mx my)
      729)
    ))
    
    
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(begin-for-test
  (local
    ((define m (new Model%))
     (define vc (new VelocityController% [model m])))
    (send vc after-key-event "right")
    (send vc details-image)
    (send vc after-button-down 300 250)
    (send vc after-key-event "right")
    (send vc after-key-event "left")
    (send vc after-key-event "left")
     (send vc after-key-event "up")
    
    (send vc after-key-event "down")
    (send vc after-key-event "down")
    
    (send vc model-after-button-down 300 250)
    (send vc model-after-drag 100 100)
    (send vc model-after-button-up  100 100)
    (send vc details-image)
    (send vc bgrnd-image (empty-scene 100 100))
    )) 
    