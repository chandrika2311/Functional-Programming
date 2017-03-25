#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "XYController.rkt")
(require "PositionController.rkt")
(require "XController.rkt")
(require "Interfaces.rkt")
(require "YController.rkt")
(require "WidgetWorks.rkt")
(require "World.rkt")
(require "VelocityController.rkt")
(require "Model.rkt")

(provide ControllerOutlet%)

;; The controller outlet implements the ControllerOutlet% class
;; It responds to keyevents and add controllers to the world.

;; A ControllerOutlet% is a (new ControllerFactory [w World<%>] [m Model<%>])
;; INTERP:
;; -- w is the World in which the controllers will be added to
;; -- m is the Model to which the controllers will be connected to
(define ControllerOutlet%
  (class* object% (SWidget<%>)
    
    ; the world in which the controllers will live
    (init-field w) ; World<%>
    
    ; the model to which the controllers will be connected
    (init-field m) ; Model<%>
    
    (super-new)
    
    ;; add-details-viewer: Container -> Void
    ;; GIVEN: A controller class that is subclass of ControllerContainer<%>
    ;; EFFECT: Adds a controller object to the current container canvas
    ;; EXAMPLES: (send add-details-controller XController%)-> Adds the x controller to
    ;;               ListOfWidgets in the Conatiner Canvas
    (define/public (add-details-controller cont)
      (send w add-widget (new cont [model m])))
    
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: A KeyEvent
    ;; EFFECT: Adds the respective controller type to the World
    ;; EXAMPLES: (send after-key-event "v")
    ;;           -> adds a new Velocity Controller to the World
    ;; STRATEGY: Cases on KeyEvents kev
    (define/public (after-key-event ke)
      (cond
        [(key=? ke "p") (add-details-controller PositionController%)]
        [(key=? ke "v") (add-details-controller VelocityController%)]
        [(key=? ke "x") (add-details-controller XController%)]
        [(key=? ke "y") (add-details-controller YController%)]
        [(key=? ke "z") (add-details-controller XYController%)]))
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: A scene
    ;; RETURNS: The given scene is returned
    ;; EXAMPLES: (send add-to-scene (empty-scene 400 500)) -> (empty-scene 400 500)
    (define/public (add-to-scene scene) scene)
    
    ;; after-tick: -> void
    ;; GIVEN: no arguments
    ;; EFFECT: no change in ControllerFactory after tick
    (define/public (after-tick) 'no-change-in-controller-factory-tick)
    
    ;; after-button-down: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: no change in controller factory on button down
    (define/public (after-button-down mx my)
      'no-change-in-controller-factory-button-down)
    
    ;; after-drag: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: no change in controller factory after drag
    (define/public (after-drag mx my)
      'no-change-in-controller-factory-drag)
    
    ;; after-button-up: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: no change in controller factory after button up
    (define/public (after-button-up mx my)
      'no-change-in-controller-factory-button-up)
    
    
    ;; after-move: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: no change in controller factory after move
    (define/public (after-move mx my)
      'no-change-in-controller-factory-button-up)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (local
    ((define model (new Model%));
     (define  world (make-world model))
     (define co1 (new ControllerOutlet% [w world][m model])))
     (send co1 after-tick)
    (send co1 after-key-event "v")
    (send co1 after-key-event "p")
    (send co1 after-key-event "z")
    (send co1 after-key-event "x")
    (send co1 after-key-event "y")
    (send co1 add-to-scene (empty-scene 210 110))
    (send co1 after-button-down 110 110)
    (send co1 after-drag 120 120)
    (send co1 after-button-up 120 120)
    (send co1 after-key-event "p")
    (send co1 after-key-event "z")
    (send co1 after-key-event "a")
    (send co1 after-move 201 205)
    ))