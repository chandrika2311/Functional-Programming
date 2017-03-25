;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(check-location "07" "q1.rkt")
(require "extras.rkt")
(provide probe-possible-outcome?
         make-turn-right
         make-turn-left
         make-move-forward)


;;DATA-DEFINITION:
#|
A Program is a ListOfInstruction
Interp: A sequence of instructions, to be executed from left to
right. 

An Instruction is one of
-- (make-turn-left)            Interp: a turn-left instruction
-- (make-turn-right)           Interp: a turn-right instruction
-- (make-move-forward PosInt)  Interp: an instruction to move forward
                                       the given number of steps.
-------------------------------------------------------------------
|#

(define-struct turn-right())
(define-struct turn-left())
(define-struct move-forward (n))

; A Direction is one of these Strings:
;;   --NORTH
;;   --SOUTH
;;   --EAST
;;   --WEST
;;Interpretation: self evident

;;Template:
;;Direction-fn : direction ->??
#|
(define ( direction-fn p)
  (cond[(string=? NORTH)...]
       [(string=? SOUTH)...]
       [(string=? EAST)...]
       [(string=? WEST)...]
))
-------------------------------------------------------------------
|#

;CONSTANTS DEFINITIONS:
(define NORTH "north")
(define SOUTH "south")
(define EAST "east")
(define WEST "west")
(define-struct probe( x1 x2 y1 y2 direction))
;;A Probe is a
;;(make-probe String String Direction)
;; Interpretation:
;;                x1         : position on probe on the x axis of x-y plane(graphics)
;;                x2         : position on probe on the x axis of x-y plane(graphics)
;;                y1         : position on probe on the y axis of x-y plane(graphics)
;;                y2         : position on probe on the y axis of x-y plane(graphics)
;;                Direction : Direction of the probe
                            
;;TEMPLATE:
;;probe-fn: Probe -> ??

#|
(define ( probe-fn p)
  (...
   (probe-x1 p)
   (probe-x2 p)
   (probe-y1 p)
   (probe-y2 p)
   (probe-direction p)
   ))

A Program is a ListOfInstruction
Interp: A sequence of instructions, to be executed from left to
right. 

An Instruction is one of
-- (probe-turn-left)            Interp: a turn-left instruction
-- (probe-turn-right)           Interp: a turn-right instruction
-- (make-move-forward PosInt)  Interp: an instruction to move forward
                                       the given number of steps.
|#
#|

---------------------------------------------------------------------
GIVEN: A Probe
RETURNS: A Probe like the original, but turned 90 degrees left
probe-turn-left : Probe -> Probe

STRATEGY: Use of Template for Probe p on direction

EXAMPLE:
(probe-turn-left (make-probe 1 -2 NORTH))=>(make-probe 1 -2  WEST)
(probe-turn-left (make-probe 2 -5 WEST))=>(make-probe 1 -2  SOUTH)

|#
(define(probe-turn-left  p )
 (cond[(string=? (probe-direction p) NORTH)
       (make-probe (probe-x1 p)(probe-x2 p)(probe-y1 p)(probe-y2 p) WEST) ]
      [(string=? (probe-direction p) WEST)
       (make-probe (probe-x1 p)(probe-x2 p)(probe-y1 p)(probe-y2 p) SOUTH) ]
      [(string=? (probe-direction p) SOUTH)
       (make-probe (probe-x1 p)(probe-x2 p)(probe-y1 p)(probe-y2 p) EAST) ]
      [(string=? (probe-direction p) EAST)
       (make-probe (probe-x1 p)(probe-x2 p)(probe-y1 p)(probe-y2 p) NORTH) ]))

;;------------------------------Test Cases
(begin-for-test
  (check-equal?
   (probe-turn-left (make-probe 1 -2 3 3 NORTH)) (make-probe 1 -2 3 3  WEST) "Not an expected answer")
  (check-equal?
   (probe-turn-left (make-probe 2 -5 3 3 WEST)) (make-probe 2 -5 3 3  SOUTH)"Not an expected answer")
  (check-equal?
   (probe-turn-left (make-probe 1 -2 3 3 SOUTH)) (make-probe 1 -2 3 3  EAST) "Not an expected answer")
  (check-equal?
  (probe-turn-left (make-probe 1 -2 3 3 EAST)) (make-probe 1 -2 3 3  NORTH) "Not an expected answer"))

#|
GIVEN: A Probe
RETURNS: A Probe like the original, but turned 90 degrees left
probe-turn-right : Probe -> Probe
EXAMPLE:
(probe-turn-right (make-probe 1 -2 NORTH))=>(make-probe 1 -2  EAST)
(probe-turn-right (make-probe 2 -5 EAST))=>(make-probe 1 -2  SOUTH)
STRATEGY: Use of Template for Probe p on direction
|#
(define(probe-turn-right p )
  (cond[(string=? (probe-direction p) NORTH)
        (make-probe (probe-x1 p)(probe-x2 p)(probe-y1 p)(probe-y2 p) EAST) ]
       [(string=? (probe-direction p) SOUTH)
        (make-probe (probe-x1 p)(probe-x2 p)(probe-y1 p)(probe-y2 p) WEST) ]
       [(string=? (probe-direction p) EAST)
        (make-probe (probe-x1 p)(probe-x2 p)(probe-y1 p)(probe-y2 p) SOUTH) ]
       [(string=? (probe-direction p) WEST)
        (make-probe (probe-x1 p)(probe-x2 p)(probe-y1 p)(probe-y2 p) NORTH) ]
                          ))
;;------------------------------Test Cases
(begin-for-test
  (check-equal?
   (probe-turn-right (make-probe 1 -2 3 3 NORTH)) (make-probe 1 -2 3 3 EAST) "Not an expected answer")
  (check-equal?
   (probe-turn-right (make-probe 2 -5 3 3 EAST)) (make-probe 2 -5 3 3 SOUTH) "Not an expected answer")
  (check-equal?
   (probe-turn-right (make-probe 1 -2 3 3 SOUTH)) (make-probe 1 -2 3 3 WEST) "Not an expected answer")
  (check-equal?
   (probe-turn-right (make-probe 1 -2 3 3 WEST)) (make-probe 1 -2 3 3 NORTH) "Not an expected answer")
  )


;;move-probe-forward PosInt: probe Steps-> Probe

;;GIVEN: Probe = probe's initial state 
;;        i = Steps to be taken by probe in the current direction
;;RETURNS: Probe's new posiiton after taking steps i
;;STRATEGY: Use template for PROBE on P
(define (move-probe-forward p i)
  (cond[(equal?(probe-direction p) NORTH)
        (make-probe (probe-x1 p)
                    (probe-x2 p)
                    (- (probe-y1 p)(+ i 2))
                    (- (probe-y2 p)(- i 2))
                    (probe-direction p))]
       [(equal?(probe-direction p) SOUTH)
        (make-probe (probe-x1 p)
                    (probe-x2 p)
                    (+ (probe-y1 p)(- i 2))
                    (+ (probe-y2 p)(+ i 2))
                    (probe-direction p))]
       [(equal?(probe-direction p) EAST)
        (make-probe (+ (probe-x1 p)(- i 2))
                    (+ (probe-x2 p)(+ i 2))
                    (probe-y1 p)
                    (probe-y2 p)
                    (probe-direction p))]
       [(equal?(probe-direction p) WEST)
        (make-probe (- (probe-x1 p)(+ i 2))
                    (- (probe-x2 p)(- i 2))
                    (probe-y1 p)
                    (probe-y2 p)
                    (probe-direction p))]))

(define probe1 (make-probe 0 0 0 0 NORTH))
(define probe2 (make-probe 0 0 -12 -8 NORTH))
(define probe3 (make-probe 0 0 0 0 SOUTH))
(define probe4 (make-probe 0 0 8 12 SOUTH))
(define probe5 (make-probe 8 12 8 12 EAST))
(define probe6 (make-probe 16 24 8 12 EAST))
(define probe7 (make-probe 0 0 0 0 WEST))
(define probe8 (make-probe -12 -8 0 0 WEST))

(begin-for-test(check-equal?(move-probe-forward probe1 10 )probe2)
               (check-equal?(move-probe-forward probe3 10 )probe4)
               (check-equal?(move-probe-forward probe3 10 )probe4)
               (check-equal?(move-probe-forward probe5 10 )probe6)
               (check-equal?(move-probe-forward probe7 10 )probe8))

;; probe-possible-outcome? : Int Int Program Int Int -> Bool
;; GIVEN: starting coordinates x0, y0, a robot program p, and ending
;; coordinates x1, y1.
;; RETURNS: true iff the robot, starting at (x0, y0) and facing north,
;; and executing program p according to the tolerances given above,
;; could end at (x1, y1).
;; EXAMPLES:
;; Let p1 = (list (probe-turn-right)
;;                (make-move-forward 10)
;;                (probe-turn-right)
;;                (make-move-forward 5))
;; then (probe-possible-outcome 20 100 p1 x1 y1) = true iff
;; x1 is in the interval [28, 32] and y1 is in the interval
;; [103,107].
;;STRATEGY: Use template for PROBE on P
(define (probe-possible-outcome? x0 y0 p x1 y1)
 (in-range? (running-program-on-probe (make-probe x0 x0 y0 y0 NORTH) p) x1 y1))
(begin-for-test(check-equal?(probe-possible-outcome? 20 100 program1 28 104)#t))
(begin-for-test(check-equal?(probe-possible-outcome? 20 100 program1 10 104)#f))
(begin-for-test(check-equal?(probe-possible-outcome? 20 100 program1 28 108)#f))



;;running-program-on-probe: Probe Program
;;GIVEN: An initial Probe and a set of Program instructions that the probe should follow
;;RETURNS:A probe after all the instructions in the program have been followed by the probe
;;EXAMPLE:
;;STRATEGY:
(define(running-program-on-probe probe p)
  (cond[(empty? p) probe]
       [else (running-program-on-probe(perform-instructions-on-probe (first p)probe)
                                      (rest p))]))


(begin-for-test(check-equal?(running-program-on-probe (make-probe 0 0 0 0 NORTH) program1)
                            (make-probe 8 12 3 7 SOUTH)))
(begin-for-test(check-equal?(running-program-on-probe (make-probe 0 0 0 0 NORTH) program2)
                            (make-probe 8 12 -7 -3 NORTH)))
(begin-for-test(check-equal?(running-program-on-probe (make-probe 0 0 0 0 NORTH)
                                                      (list(make-turn-right)
                                                           (make-move-forward 10)))
                            (make-probe 8 12 0 0 EAST))
               (check-equal?(running-program-on-probe (make-probe 0 0 0 0 NORTH)
                                                      (list(make-turn-right)
                                                           (make-move-forward 10)
                                                           (make-turn-right)
                                                           (make-move-forward 10)))
                            (make-probe 8 12 8 12 SOUTH))
               (check-equal?(running-program-on-probe (make-probe 0 0 0 0 NORTH)
                                                      (list(make-turn-right)
                                                           (make-move-forward 10)
                                                           (make-turn-right)
                                                           (make-move-forward 10)
                                                           (make-turn-left)
                                                           (make-turn-left)))
                            (make-probe 8 12 8 12 NORTH))
               (check-equal?(running-program-on-probe (make-probe 0 0 0 0 NORTH)
                                                      (list(make-turn-right)
                                                           (make-move-forward 10)
                                                           (make-turn-right)
                                                           (make-move-forward 10)
                                                           (make-turn-left)
                                                           (make-turn-left)
                                                           (make-turn-right)
                                                           (make-move-forward 10)))
                            (make-probe 16 24 8 12 EAST))
               (check-equal?(running-program-on-probe (make-probe 0 0 0 0 NORTH)
                                                      (list(make-turn-right)
                                                           (make-move-forward 10)
                                                           (make-turn-right)
                                                           (make-move-forward 10)
                                                           (make-turn-left)
                                                           (make-turn-left)
                                                           (make-move-forward 10)))
                            (make-probe 8 12 -4 4 NORTH))
               (check-equal?(running-program-on-probe (make-probe 0 0 0 0 NORTH)
                                                      (list(make-turn-right)
                                                           (make-move-forward 10)
                                                           (make-turn-right)
                                                           (make-move-forward 10)
                                                           (make-turn-left)
                                                           (make-turn-left)
                                                           (make-move-forward 10)
                                                           (make-turn-right)))
                            (make-probe 8 12 -4 4 EAST)))
;_____________________________________________________________________
;;perform-instructions-on-probe: Instruction Program Probe
;;GIVEN:Instruction from the program
;;      Program containing the rest of the instructions
;;      Probe that should act on the instructions
;;RETURNS: Probe after the instructions have been performed
;;EXAMPLE:
;;STRATEGY:
(define(perform-instructions-on-probe instruction probe)
  (cond
    [(turn-left? instruction) (probe-turn-left probe)]
    [(turn-right? instruction) (probe-turn-right probe)]
    [(move-forward? instruction) (move-probe-forward probe (move-forward-n instruction))]
    ))

;;Tests:
(begin-for-test(check-equal?(perform-instructions-on-probe (make-turn-right)
                                                           
                                                           (make-probe 0 0 0 0 NORTH))
                            (make-probe 0 0 0 0 EAST)))


;_____________________________________________________________________
;;in-range? Probe x1 y1
;;GIVEN: Probe after instruction have been executed
;;RETURN:
(define(in-range? pr x1 y1)
  (and(>= x1 (probe-x1 pr))
      (<= x1 (probe-x2 pr))
      (>= y1 (probe-y1 pr))
      (<= y1 (probe-y2 pr))))
(begin-for-test(check-equal? (in-range? (make-probe 8 12 3 7 SOUTH) 9 5)#t))
;_____________________________________________________________________
(define program1(list (make-turn-right)
                (make-move-forward 10)
                (make-turn-right)
                (make-move-forward 5)))
(define program3(list
                (make-turn-right)
                (make-move-forward 10)
                (make-turn-right)
                (make-move-forward 10)
                (make-turn-left)
                (make-turn-left)
                (make-turn-right)
                (make-move-forward 10)
                (make-turn-right)
                (make-move-forward 10)
                (make-turn-left)
                (make-turn-left)))
(define program2(list (make-turn-right)
                (make-move-forward 10)
                (make-turn-left)
                (make-move-forward 5)))
