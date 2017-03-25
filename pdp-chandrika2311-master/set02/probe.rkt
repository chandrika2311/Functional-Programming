;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(check-location "02" "probe.rkt")
(require "extras.rkt")
(provide probe-at
         probe-turned-left
         probe-turned-right
         probe-direction-equal?
         probe-location-equal?
         probe-forward-possible-outcome?)


;;DATA-DEFINITION:

; A Direction is one of these Strings:
;;   --"North"
;;   --"South"
;;   --"East"
;;   --"West"
;;Interpretation: self evident

;;Template:
;;Direction-fn : direction ->??
#|
(define ( direction-fn p)
  (cond[(string=? "North")...]
       [(string=? "South")...]
       [(string=? "East")...]
       [(string=? "West")...]
))
|#

(define-struct probe( x y direction))
;;A Probe is a
;;(make-probe String String Direction)
;; Interpretation:
;;                x         : position on probe on the x axis of x-y plane(graphics)
;;                y         : position on probe on the y axis of x-y plane(graphics)
;;                Direction : Direction of the probe
                            
;;TEMPLATE:
;;probe-fn: Probe -> ??

#|
(define ( probe-fn p)
  (...
   (probe-x p)
   (probe-y p)
   (probe-direction p)
   ))
|#

#|
-------------------------------------------------------------------
probe-at: Integer Integer -> Probe
GIVEN: an x-coordinate and a y-coordinate
RETURNS: a probe with its center at those coordinates, facing north.
STRATEGY: Combining simpler functions

EXAMPLE:
(probe-at 1 -3) = (make-probe 1 -3 "North")
(probe-at 2 -2) = (make-probe 2 -2 "North")
(probe-at 5 -3) = (make-probe 5 -3 "North")


|#
(define (probe-at x y)
  (make-probe x y "North")
   )

;;------------------------------Test Cases
(begin-for-test
  (check-equal? (probe-at 1 -3) (make-probe 1 -3 "North") "Not an expected answer")
  (check-equal? (probe-at 2 -2) (make-probe 2 -2 "North") "Not an expected answer")
  (check-equal? (probe-at 5 -3) (make-probe 5 -3 "North") "Not an expected answer"))
  
#|
---------------------------------------------------------------------
GIVEN: A Probe
RETURNS: A Probe like the original, but turned 90 degrees left
probe-turned-left : Probe -> Probe

STRATEGY: Use of Template for Probe p on direction

EXAMPLE:
(probe-turned-left (make-probe 1 -2 "North"))=>(make-probe 1 -2  "West")
(probe-turned-left (make-probe 2 -5 "West"))=>(make-probe 1 -2  "South")

|#
(define(probe-turned-left  p )
                         (cond[(string=? (probe-direction p) "North") (make-probe (probe-x p) (probe-y p) "West") ]
                              [(string=? (probe-direction p) "West") (make-probe (probe-x p) (probe-y p) "South") ]
                              [(string=? (probe-direction p) "South") (make-probe (probe-x p) (probe-y p) "East") ]
                              [(string=? (probe-direction p) "East") (make-probe (probe-x p) (probe-y p) "North") ]
                          ))

;;------------------------------Test Cases
(begin-for-test
  (check-equal? (probe-turned-left (make-probe 1 -2 "North")) (make-probe 1 -2  "West") "Not an expected answer")
  (check-equal? (probe-turned-left (make-probe 2 -5 "West")) (make-probe 2 -5  "South")"Not an expected answer"))

#|



GIVEN: A Probe
RETURNS: A Probe like the original, but turned 90 degrees left
probe-turned-right : Probe -> Probe
EXAMPLE:
(probe-turned-right (make-probe 1 -2 "North"))=>(make-probe 1 -2  "East")
(probe-turned-right (make-probe 2 -5 "East"))=>(make-probe 1 -2  "South")
STRATEGY: Use of Template for Probe p on direction
|#
(define(probe-turned-right p )
                         (cond[(string=? (probe-direction p) "North") (make-probe (probe-x p) (probe-y p) "East") ]
                              [(string=? (probe-direction p) "East") (make-probe (probe-x p) (probe-y p) "South") ]
                              [(string=? (probe-direction p) "South") (make-probe (probe-x p) (probe-y p) "West") ]
                              [(string=? (probe-direction p) "West") (make-probe (probe-x p) (probe-y p) "North") ]
                          ))
;;------------------------------Test Cases
(begin-for-test
  (check-equal? (probe-turned-right (make-probe 1 -2 "North")) (make-probe 1 -2 "East") "Not an expected answer")
  (check-equal? (probe-turned-right (make-probe 2 -5 "East")) (make-probe 2 -5 "South") "Not an expected answer")
  )


#|
probe-direction-equal? : Probe Probe -> Boolean
GIVEN: two probes
RETURNS: true iff the two probes are facing in the same direction,
else false
EXAMPLE:
(probe-direction-equal? (make-probe 1 -2 "North")(make-probe 2 -5 "East"))=> #f
(probe-direction-equal? (make-probe 1 -2 "North")(make-probe 2 -5 "North"))=> #t
STRATEGY: Use of Template for Probe p
|#
(define (probe-direction-equal? p1 p2)
  ( cond[(equal? (probe-direction p1) (probe-direction p2)) #t]
        [else #f]
        ))

;;------------------------------Test Cases
(begin-for-test
  (check-equal? (probe-direction-equal? (make-probe 1 -2 "North")(make-probe 2 -5 "East") ) #f "Not expected answer")
  (check-equal? (probe-direction-equal? (make-probe 1 -2 "North")(make-probe 2 -5 "North") ) #t "Not expected answer"))
  
  


#|
probe-location-equal? : Probe Probe -> Boolean
GIVEN: two probles
RETURNS: true iff the two probes are at the same location
EXAMPLES:
(probe-location-equal? (make-probe 1 -2 "North") (make-probe 1 -2 "East")) => #t
(probe-location-equal? (make-probe 1 -3 "North") (make-probe 1 -2 "East")) => #f
STRATEGY: Use of Template for Probe p on x and y

|#

(define (probe-location-equal? p1 p2)
  ( cond[ (and (equal? (probe-x p1) (probe-x p2))(equal? (probe-y p1) (probe-y p2))) #t]
        [else #f]
        ))
;;------------------------------Test Cases
(begin-for-test
  (check-equal? (probe-location-equal? (make-probe 1 -2 "North") (make-probe 1 -2 "East")) #t "Not expected answer")
  (check-equal? (probe-location-equal? (make-probe 1 -3 "North") (make-probe 1 -2 "East")) #f "Not expected answer"))


#|
probe-forward-possible-outcome? : Probe PosInt Probe -> Boolean
GIVEN: two probes and a distance
RETURNS: true iff the first probe, given a move-forward command with
the specified number of steps, could wind up in the state described by
the second probe.
EXAMPLE:
(probe-forward-possible-outcome? (make-probe 1 -6 "East") 2 (make-probe 3 -6 "East"))=> #t
(probe-forward-possible-outcome? (make-probe 1 -6 "East") 2 (make-probe 6 -6 "East"))=> #f
(probe-forward-possible-outcome? (make-probe 5 -6 "West") 1 (make-probe 4 -6 "West"))=> #t
(probe-forward-possible-outcome? (make-probe 5 -6 "West") 1 (make-probe 1 -6 "West"))=> #f
STRATEGY: Use of Template for Probe p on direction

|#
(define (probe-forward-possible-outcome? p1 i p2)
  (cond[(equal? (probe-direction p1) "North")(move_north? p1 i p2)];Probe will move north which means that y decreases
       [(equal? (probe-direction p1) "South")(move_south? p1 i p2)];Probe willl move south which means y increases 
       [(equal? (probe-direction p1) "East")(move_east? p1 i p2)]  ;Probe will move east which means x increases
       [(equal? (probe-direction p1) "West")(move_west? p1 i p2)]  ;Probe will move West which means x decreases
))


;;-------------------------------------Test Cases
(begin-for-test
  (check-equal? (probe-forward-possible-outcome? (make-probe 1 -2 "North")  3 (make-probe 1 -4 "North")) #t)
  (check-equal? (probe-forward-possible-outcome? (make-probe 1 -2 "North")  3 (make-probe 1 -7 "North")) #f)
  (check-equal? (probe-forward-possible-outcome? (make-probe 1 -6 "South") 3 (make-probe 1 -3 "South")) #t)
  (check-equal? (probe-forward-possible-outcome? (make-probe 1 -6 "South") 3 (make-probe 1 -5 "South")) #f)
  (check-equal? (probe-forward-possible-outcome? (make-probe 1 -6 "East") 2 (make-probe 3 -6 "East")) #t)
  (check-equal? (probe-forward-possible-outcome? (make-probe 1 -6 "East") 2 (make-probe 6 -6 "East")) #f)
  (check-equal? (probe-forward-possible-outcome? (make-probe 5 -6 "West") 1 (make-probe 4 -6 "West")) #t)
  (check-equal? (probe-forward-possible-outcome? (make-probe 5 -6 "West") 1 (make-probe 1 -6 "West")) #f))


#|
move_north? : Probe PosInt Probe -> Boolean
GIVEN: two probes and a distance
RETURNS: true iff the first probe , given a move-forward command with
the specified number of steps towards North, could wind up in the state described by
the second probe.
EXAMPLE:
( move_north? (make-probe 1 -6 "North") 2 (make-probe 3 -6 "North"))=> #t
( move_north? (make-probe 1 -2 "North")  3 (make-probe 1 -7 "North"))=> #f
STRATEGY: Use of Template for Probe p on x and y
|#
(define(move_north? p1 i p2)
  (cond[(and(equal? (probe-x p2) (probe-x p1)) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(-(probe-y p1) i)) ) #t]
   [(and(equal? (probe-x p2) (probe-x p1)) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(-(probe-y p1)(+ i 1))) ) #t]
   [(and(equal? (probe-x p2) (probe-x p1)) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(-(probe-y p1)(- i 1))) ) #t]
   [else #f]))
;;------------------------------Test Cases
(begin-for-test
  (check-equal? ( move_north? (make-probe 3 -2 "North") 3 (make-probe 3 -4 "North")) #t )
  (check-equal? ( move_north? (make-probe 3 -2 "South") 3 (make-probe 3 -4 "North")) #f )
  (check-equal? ( move_north? (make-probe 1 -2 "North")  3 (make-probe 1 -7 "West")) #f ))

#|
move_south? : Probe PosInt Probe -> Boolean
GIVEN: two probes and a distance
RETURNS: true iff the first probe , given a move-forward command with
the specified number of steps towards South, could wind up in the state described by
the second probe.
EXAMPLE:
(move_south? (make-probe 1 -6 "South") 3 (make-probe 1 -3 "South"))=> #t
(move_south? (make-probe 1 -6 "South") 3 (make-probe 1 -5 "South"))=> #f
STRATEGY: Use of Template for Probe p on x and y
|#
(define(move_south? p1 i p2)
  (cond[(and(equal? (probe-x p2) (probe-x p1)) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(+(probe-y p1) i)) ) #t]
   [(and(equal? (probe-x p2) (probe-x p1))(equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(+(probe-y p1)(+ i 1))) ) #t]
   [(and(equal? (probe-x p2) (probe-x p1))(equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(+(probe-y p1)(- i 1))) ) #t]
   [else #f]))

;;------------------------------Test Cases
(begin-for-test
  (check-equal? ( move_south? (make-probe 1 -6 "South") 3 (make-probe 1 -3 "South")) #t )
  (check-equal? ( move_south? (make-probe 1 -6 "South") 3 (make-probe 1 -5 "South")) #f ))


#|
move_east? : Probe PosInt Probe -> Boolean
GIVEN: two probes and a distance
RETURNS: true iff the first probe , given a move-forward command with
the specified number of steps towards East, could wind up in the state described by
the second probe.
EXAMPLE:
( move_east? (make-probe 1 -6 "East") 2 (make-probe 3 -6 "East"))=> #t
( move_east? (make-probe 1 -6 "East") 2 (make-probe 6 -6 "East"))=> #f
STRATEGY: Use of Template for Probe p on x and y
|#
(define(move_east? p1 i p2)
  (cond[(and(equal? (probe-x p2) (+(probe-x p1) i)) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(probe-y p2))) #t]
       [(and(equal? (probe-x p2) (+(probe-x p1)(+ i 1))) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(+(probe-y p2)(+ i 1))))  #t]
       [(and(equal? (probe-x p2) (+(probe-x p1)(- i 1))) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(+(probe-y p2)(- i 1))))  #t]
       [else #f]))

;;------------------------------Test Cases
(begin-for-test
  (check-equal? ( move_east? (make-probe 1 -6 "East") 2 (make-probe 3 -6 "East")) #t )
  (check-equal? ( move_east? (make-probe 1 -6 "East") 2 (make-probe 6 -6 "East")) #f ))

#|
move_west? : Probe PosInt Probe -> Boolean
GIVEN: two probes and a distance
RETURNS: true iff the first probe , given a move-forward command with
the specified number of steps towards West, could wind up in the state described by
the second probe.
EXAMPLE:
( move_west? (make-probe 5 -6 "West") 1 (make-probe 4 -6 "West"))=> #t
( move_west? (make-probe 5 -6 "West") 1 (make-probe 1 -6 "West"))=> #f
STRATEGY: Use of Template for Probe p on x and y
|#
(define(move_west? p1 i p2)
  (cond[(and(equal? (probe-x p2) (-(probe-x p1) i )) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(probe-y p2))) #t];5 , 5-1, 5+1
       [(and(equal? (probe-x p2) (-(probe-x p1)(+ i 1))) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(+(probe-y p2)(+ i 1))))  #t]
       [(and(equal? (probe-x p2) (-(probe-x p1)(- i 1))) (equal? (probe-direction p2) (probe-direction p1)) (equal? (probe-y p2)(+(probe-y p2)(- i 1))))  #t]
       [else #f]))

;;------------------------------Test Cases
(begin-for-test
  (check-equal? ( move_west? (make-probe 5 -6 "West") 1 (make-probe 4 -6 "West")) #t )
  (check-equal? ( move_west? (make-probe 5 -6 "West") 1 (make-probe 1 -6 "West")) #f ))



