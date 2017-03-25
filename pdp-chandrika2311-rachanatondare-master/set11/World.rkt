#lang racket

(require "interfaces.rkt")
(require "WidgetWorks.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide World% make-world)


;; A World is a (make-world model) 

;; make-world : Model -> World
;; GIVEN  : a Model Object
;; Return : a world with given model and empty widget list.
;; Strategy : combine simpler functions.
(define (make-world m)
  (new World% [model m]))

;; A World% consists of a model and stateful widgets.  
;; It sends after-tick,mouse events and keyboard events,add-to-scene
;; to the model and to all controllers

(define World%
  (class* object%
    (SWidget<%>)

    (init-field model) ; model object. Refer class model for more details 
    (init-field [widgets empty])   ; list of SWidgets initialized with empty.

    ;; (Widget -> Void) -> Void
    (define (for-each-widget fn)
      (for-each fn widgets))

    ;; (Widget Y -> Y) Y ListOfWidget -> Y
    (define (foldr-widgets fn base)
      (foldr fn base widgets)) 

    (super-new)

    ;; after-tick : -> void
    ;; GIVEN  : no arguments
    ;; EFFECT : updates this widget to the
    ;;          state it should have following a tick.
     (define/public (after-tick)
      (send model after-tick)
      (for-each (lambda (wid) (send wid after-tick)) widgets))

    
    ;; to-scene : Scene -> Scene
    ;; GIVEN    : a scene.
    ;; RETURNS  : the same scene with all the slbocks in this metatoy
    ;;            painted on it.
    ;; STRATEGY : Use HOFC foldr on the SWidgets in this Metatoy
    (define/public (add-to-scene myscene)
      (foldr
        (lambda (wid base)
          (send wid add-to-scene base)) 
        myscene
        widgets))
    


    ;; after-mouse-event : NonNegInt NonNegInt MouseEvent -> Metatoy
    ;; GIVEN   : two non negative integers indicating x and y position of 
    ;;           the mouse and a MouseEvent.
    ;; EFFECT  : a world which follows the given MouseEvent
    ;; STRATGY : Cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (after-button-down mx my)] 
        [(mouse=? mev "drag")
         (after-drag mx my)]
        [(mouse=? mev "button-up")
         (after-button-up mx my)]
        [else (after-move mx my)]))

    
    ;; after-button-down : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : a World which should follow the MouseEve nt when the mouse
    ;;            is pressed down at the given mouse position
    ;; STRATEGY : Use HOF on toys
    (define/public (after-button-down mx my)
     (for-each
        ;;  SWidget -> SWidget
        ;; RETURNS : the same toy but after after-button-down event.
        (lambda (wid)
          (send wid after-button-down mx my))
        widgets))
       
    ;; after-button-up : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : a World which should follow the MouseEvent when the mouse
    ;;            is depressed at the given mouse position.
    ;; STRATEGY : Use HOF on toys
    (define/public (after-button-up mx my)
       (for-each
        ;; SWidget -> Void
        ;; Effect : the send the after-button-up to widget
        (lambda (wid) 
          (send wid after-button-up mx my))
        widgets))
     
    
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : a World which should follow the MouseEvent
    ;;            when the mouse is dragged at the given mouse position.
    ;; STRATEGY : Use HOF on toys 
    (define/public (after-drag mx my)
      (for-each
        ;; SWidget -> Void
        ;; Effect : the send the after-drag to widget
        (lambda (wid) 
          (send wid after-drag mx my))
        widgets))
        
    
    ;; after-move : NonNegInt NonNegInt -> Metatoy
    ;; Given    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : a World which should follow the MouseEvent
    ;             when there is any other mouse event like drag, button-down,
    ;;            or button-up.
    ;; STRATEGY : Use HOF on toys
    (define/public (after-move mx my)
      (for-each
        ;; SWidget -> Void
        ;; Effect : the send the after-move to widget
        (lambda (wid) 
          (send wid after-move mx my)) 
        widgets))

        
    ;; after-key-event : KeyEvent -> Void
    ;; RETURNS  : a Metatoy based on KeyEvent 'b' which create
    ;;            sblock.
    ;; STRATEGY : Cases on key event - kev
    (define/public (after-key-event kev)
     (for-each
        ;; SWidget -> Void
        ;; Effect : the send the after-key-event to widget
        (lambda (wid) 
          (send wid after-key-event kev)) 
        widgets))


    (define/public (add-widget c)
      (set! widgets (cons c widgets)))

    ))

