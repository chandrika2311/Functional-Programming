#lang racket


;; interfaces for MVC example

(require "WidgetWorks.rkt")

(provide Controller<%>
         Model<%>
         HandleController<%>
		 World<%>)



;; A Controller is an object of any class that implements
;; Controller<%>

;; Each controller object implements Controller<%> interface

(define Controller<%>    
  (interface (SWidget<%>)

    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller
    ;; accordingly 
    recieve-signal
    
    ))

;; A Model is an object of any class that implements Model<%>.  Models
;; will receive signals from the World
(define Model<%>
  (interface ()

    ;; -> Void
    after-tick      
    
    ;; Controller -> Void
    ;; Registers the given controller to receive signal
    register          

    ;; Command -> Void
    ;; Executes the given command
    execute-command   
))

;; A World is an object of any class that implements World<%>.  World
;; will receive widgets from the Container, so they must be added to the world
(define World<%>
  (interface ()

    ;; add-widget : SWidget -> Void
       add-widget ; all the widgets are stateful

    ;; run : PosReal -> Void
       run

    ))

;; CONTROLLER HANDLE:

;; Every controller implements HandleController<%>
;; It consists of abstract methods that are requried by each subclass

;; protocol: 
;; model sends the controller an initialization signal as soon as it registers.

(define HandleController<%>
  (interface()

    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: the updated model after button down
    model-after-button-down
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: the updated model after dragged to given location
    model-after-drag

    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: the updated model after button up
    model-after-button-up

    ;; -> Image
    ;; velocity and position details display of the model
    details-image

    ;; -> Image
    ;; background image of the mdoel
    bgrnd-image))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; A Command is one of 
;; -- (make-set-position x y)     
;; -- (make-incr-velocity vx vy)
;; -- (make-set-pause (boolean)

;; A Signal is one of
;; -- (make-report-position x y)
;; -- (make-report-velocity vx vy)

;; x, y ,vx ,vy, are all Reals.

;; provide the structs for Command and Signal
;; the syntax of provide in #lang racket has more options in it.
;; structs for model command language

(provide 
  (struct-out set-position) 
  (struct-out increment-velocity)
  (struct-out report-position)
  (struct-out report-velocity)
  (struct-out set-pause))

(define-struct set-position (pos-x pos-y) #:transparent)
(define-struct increment-velocity (dvx dvy) #:transparent)
(define-struct report-position (pos-x pos-y) #:transparent)
(define-struct report-velocity (vx vy) #:transparent)
(define-struct set-pause (boolean) #:transparent)

