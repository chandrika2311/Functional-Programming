#lang racket


;; the model consists of a particle. The particle, when unselected,
;; bounces with a given
;; velocity between a rectangular boundary
;; It accepts commands and reports when its status changes

(require rackunit)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "World.rkt")
(require "TestXYController.rkt")


(provide Model%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITONS

;; LISTOFCONTROLLER
;;
;; A ListOfController (loc) is either
;; --- empty
;; --- (cons Controller loc)

;; INTERPRETAION
;;
;; A ListOfController is either empty list or a collection of 
;; ListOfController

;; TEMPLATE
;;
;; loc-fn: ListOfController -> ??
;; (define (loc-fn loc)
;;   (cond
;;     [(empty? loc)]
;;     [else (.. ( Controller-fn (first loc))
;;           (loc-fn rest-loc)))]))

;; a model is a (new Model% [x Integer][y Integer]
;;                          [vx Integr][vy Integer]
;;                          [controllers ListOfController]
;;                          [pause? Boolean])

(define Model%
  (class* object% (Model<%>)

   ;; boundaries of the conatiner
   (init-field [low-x 0]) ;; least value of x-coordinate
   (init-field [high-x 150]) ;; max value of x-coordinate
   (init-field [low-y 0]) ;; least value of y-coordinate    
   (init-field [high-y 100]) ;; max value of y-coordinate    

   ;; velocties and positions of the particle
   (init-field [x(/ (+ low-x high-x) 2)]) ;; Integer
   (init-field [y(/ (+ low-y high-y) 2)]) ;; Integer
   (init-field [vx 0]) ;; Integer ;;velocity along x axis    
   (init-field [vy 0]) ;; Integer ;;velocity along y axis

    ;; set based on the particle is apused or not and by default particle
    ;; is not paused
    (init-field [pause? false]) ;; Boolean

    ;; list of controllers in the container                
    (init-field [controllers empty]) ;; ListOfController
    (field [q (make-rect low-x high-x low-y high-y)]) 

     (super-new)

     

    ;; check-boundaries: Integer Integer Integer -> Integer
    ;; GIVEN: the current coordinate value, the lower and higher limit 
    ;;        coordinate values
    ;; RETURNS: same value if it is within boundaries else value of the
    ;; boundaries it crossed
    ;; EXAMPLES: (check-boundaries 0 60 120) -> 60
    ;;           (check-boundaries 0 107 120) -> 107
    ;; STRATEGY: combine simpler fucntions
    (define (check-boundaries low value high)
      (max low (min value high)))

    ;; particle: -> Particle
    ;; GIVEN: no arguments
    ;; RETUNS: a new Particle  created
    ;; EXAMPLES: (send p1 particle) -> (make-particle 90 55 0 0)
    ;; STRATEGY: make a new particle structure
    (define (particle) (make-particle (check-boundaries low-x x high-x)
                                      (check-boundaries low-y y high-y) vx vy))

    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: object is unpaused,  and moves object by vx and vy,
    ;;         updates x y vx and vy
    ;;         reports velocity at every tick
    ;;         reports position at every tick 
    ;; STRATEGY: Cases on pause?
    (define/public (after-tick) 
      (if pause?
          "nothing"
          (begin
            (let ((p (particle-after-tick (particle) q)))
              (set! x (particle-x p))
              (set! y (particle-y p))
              (set! vx (particle-vx p))
              (set! vy (particle-vy p))
              (notify-position)
              (notify-velocity)))))

    ;; register: Controller -> Void
    ;; GIVEN: a controller
    ;; EFFECT: register the controller and report position and velocity to
    ;;         details
    (define/public (register cont)
      (begin
        (set! controllers (cons cont controllers))
        (send cont recieve-signal (make-report-position x y))
        (send cont recieve-signal (make-report-velocity vx vy))))

    ;; execute-command: Command -> Void
    ;; GIVEN: a command
    ;; EFFECT: figure out the command, execute it, and send changes to
    ;;         controllers
    ;; STRATEGY: Cases on cmd
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
          (begin
            (set! x (set-position-pos-x cmd))
            (set! y (set-position-pos-y cmd))
            (notify-position))]
        [(increment-velocity? cmd)
          (begin
            (set! vx (+ vx (increment-velocity-dvx cmd)))
            (set! vy (+ vy (increment-velocity-dvy cmd)))
            (notify-velocity))]
        [(set-pause? cmd)
          (set! pause? (set-pause-boolean cmd))]))

    ;; notify position and velocity of the controllers to details controllers

    ;; notify-velocity: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: notify the position to each controller in controllers
    ;; STRATEGY: Combine simpler functions
    (define (notify-velocity)
      (let ((ping (make-report-velocity vx vy)))
         (for-each
           ;; Controller -> Void
           ;; GIVEN:  a controller
           ;; EFFECT: sends the velocity to the controller
           (lambda (obs) (send obs recieve-signal ping)) controllers)))


    ;; notify-position: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: notify the position to each controller in controllers
    ;; STRATEGY: Combine simpler functions
    (define (notify-position)
      (let ((ping (make-report-position x y)))
         (for-each
           ;; Controller -> Void
           ;; GIVEN:  a controller
           ;; EFFECT: sends the velocity to the controller
           (lambda (obs) (send obs recieve-signal ping)) controllers)))



    
    ;; -> ListofController<%>
    ;; Returns the list of all the controllers registered to this Model
    (define/public (for-test:get-controllers)
      controllers)))          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTNG

(begin-for-test
  (local
    ((define m (new Model%))
     (define txy1 (new TestXYController% [model m]))
     (define txy2 (new TestXYController% [model m])))
    (send m after-tick)
    (send m execute-command (make-set-position 150 170))
    (send m execute-command (make-increment-velocity 5 5))
    (send m execute-command (make-set-pause false))
    (check-equal?
     (length (send m for-test:get-controllers))
     2)
    (send m execute-command
          (make-set-pause true))
    (send m after-tick)
    (check-equal?
     (send txy1 for-test:get-particle-x)
     (send txy2 for-test:get-particle-x))
     ))

            