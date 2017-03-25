;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide initial-state
         next-state
         accepting-state?
         error-state? )
(check-location "02" "fsm.rkt")
 
;;Purpose is to design a set of functions that illustrate the workings of a finite-state machine for accepting strings that exactly match the regular expression
;;(q | x)* u* (a | b)* d (e | f)*
;-----------------------------------------------------------------------------------------------------------------------------------------------
#|
;; DATA DEFINITIONS:

;; a MachineInput is one of:
;;-- "q"
;;-- "x"
;;-- "u"
;;-- "a"
;;-- "b"
;;-- "d"
;;-- "e"
;;-- "f"
;; INTERPRETATION: self-evident

;;TEMPLATE:
;;MachineInput: MachineInput  -> ??

(define (machineInput str)
 (cond
   [(string=? input "q")...]
   [(string=? input "x")...]
   [(string=? input "u")...]
   [(string=? input "a")...]
   [(string=? input "b")...]
   [(string=? input "d")...]
   [(string=? input "e")...]
   [(string=? input "f")...]))  

|#
;;;;-----------------------------------------
;;a State is one of
;;-- "State0"
;;-- "State1" 
;;-- "State2" 
;;-- "State3" 
;;-- "Error_State"
;;-- "Accepting_State"


;; INTERPRETATION:
;;  "State0" : state when no input
;;  "State1" : state when input is (q/x)*
;;  "State2" : state when input is (u)*
;;  "State3" : state when input is (a/b)*
;;  "Accepting_State" : state when input is (d) and (e/f)* after (d)
;;  "Error_State": State representing an input not following the regular expression

;;TEMPLATE:
;;state : state -> ??

;;(define State st)
;;(cond
;;   [(= state "State0")...]
;;   [(= state "State1")...]
;;   [(= state "State2")...]
;;   [(= state "State3")...]
;;   [(= state "Accepting_State")...]
;;   [(= state "Error_State")...]))

;-------------------------------------------Contract, Purpose statement and Examples
#|

initial-state : Number -> State
GIVEN: a number
RETURNS: a representation of the initial state
of your machine.  The given number is ignored.
STRATEGY:combining simpler functions

EXAMPLES:
(initial-state 1) = "State0"
(initial-state 2) = "State0"
(initial-state 3) = "State0"
(initial-state 4) = "State0"
(initial-state 5) = "State0"


|#
(define (initial-state i) "State0")
;; TESTS
(begin-for-test
  (check-equal? (initial-state 1) "State0" 
    "State0 should be the result ")
  (check-equal? (initial-state 5) "State0" 
    "State0 should be the result "))
#|


next-state : State MachineInput -> State
GIVEN: a state of the machine and a machine input
RETURNS: the state that should follow the given input.

STRATEGY: Use of template for State

EXAMPLE:

(next-state "State0" "q") = "State1"
(next-state "State1" "e") = "Error_State"
(next-state "State1" "f") = "Error_State"
(next-state "State0" "x") = "State1"


(next-state "State0" "u") = "State2"
(next-state "State2" "e") = "Error_State"
(next-state "State2" "f") = "Error_State"


(next-state "State0" "a") = "State3"
(next-state "State3" "e") = "Error_State"
(next-state "State3" "f") = "Error_State"
(next-state "State0" "b") = "State3"


(next-state "State0" "d") = "Accepting_State"
(next-state "State4" "e") = "Accepting_State"
(next-state "State4" "f") = "Accepting_State"

(next-state "State0" "e") = "Error_State"
(next-state "State0" "f") = "Error_State"


|#

(define (next-state initstate machineInput);returns acc-state
   (cond
     [ (and (or (equal? machineInput "q")
                (equal? machineInput "x"))
            (or(equal? initstate "State0")
               (equal? initstate "State1")))
       "State1" ]
    
;;Cases for u
     [(and (equal? machineInput "u")
           (or (equal? initstate "State0")
              (equal? initstate "State1")
              (equal? initstate "State2")))
          
      "State2" ]
     
     [ (and (or (equal? machineInput "q")
                (equal? machineInput "x"))
            (equal? initstate "State2"))
       "Error_State" ]

     ;;Cases for a/b*
     [(and(or(equal? machineInput "a")
             (equal? machineInput "b"))
          (or (equal? initstate "State0")
              (equal? initstate "State1")
              (equal? initstate "State2")
              (equal? initstate "State3")))
      "State3" ]

     [(and (or (equal? machineInput "q")
               (equal? machineInput "x")
               (equal? machineInput "u"))
           (equal? initstate "State3")
           )
      "Error_State" ]

     ;;Cases on d
     [(and
          (equal? machineInput "d")
          (or (equal? initstate "State0")
              (equal? initstate "State1")
              (equal? initstate "State2")
              (equal? initstate "State3")))
      "Accepting_State" ]
     
     [(and (equal? machineInput "d")
           (equal? initstate "Accepting_State"))
      "Error_State" ]
     
     [(and (or(equal? machineInput "e")
              (equal? machineInput "f"))
           (equal? initstate "Accepting_State"))
      "Accepting_State" ]
     
     [(and (or(equal? machineInput "q")
              (equal? machineInput "x")
              (equal? machineInput "u")
              (equal? machineInput "a")
              (equal? machineInput "b"))
           (equal? initstate "Accepting_State"))
      "Error_State" ]
;;Cases on e/f*
     
     [(and (or(equal? machineInput "e")
              (equal? machineInput "f"))
           (or(equal? initstate "State0")
              (equal? initstate "State1")
              (equal? initstate "State2")
              (equal? initstate "State3")))
          "Error_State" ]
         
     ))


;; TESTS
(begin-for-test
  (check-equal? (next-state "State0" "q") "State1" "State1 is the expected output")
  (check-equal? (next-state "State1" "e") "Error_State" "Error_State expected")
  (check-equal? (next-state "State1" "f")  "Error_State" "Error_State expected")
  (check-equal? (next-state "State0" "x")  "State1" "State1 is the expected output")
  (check-equal? (next-state "State0" "u") "State2" "State2 is the expected output")
  (check-equal? (next-state "State2" "q") "Error_State" "Error_State is the expected output")
  (check-equal? (next-state "State2" "x") "Error_State" "Error_State is the expected output")
  (check-equal? (next-state "State2" "a") "State3" "State3 is the expected output")
  (check-equal? (next-state "State2" "b") "State3" "State3 is the expected output")
  (check-equal? (next-state "State2" "d") "Accepting_State" "Accepting_State is the expected output")
  (check-equal? (next-state "State2" "e") "Error_State"  "Error_State expected")
  (check-equal? (next-state "State2" "f") "Error_State"  "Error_State expected")
  (check-equal? (next-state "State0" "a")  "State3" "State3 expected")
  (check-equal? (next-state "State3" "e")  "Error_State"  "Error_State expected")
  (check-equal? (next-state "State3" "f")  "Error_State"  "Error_State expected")
  (check-equal? (next-state "State3" "q")  "Error_State" "Error_State expected")
  (check-equal? (next-state "State3" "x")  "Error_State" "Error_State expected")
  (check-equal? (next-state "State3" "u")  "Error_State" "Error_State expected")
  (check-equal? (next-state "State3" "d")  "Accepting_State"  "Accepting_State expected")
  (check-equal? (next-state "State0" "d")  "Accepting_State" "Accepting_State expected")
  (check-equal? (next-state "Accepting_State" "e")  "Accepting_State" "Accepting_State expected")
  (check-equal? (next-state "Accepting_State" "f")  "Accepting_State" "Accepting_State expected")
  (check-equal? (next-state "Accepting_State" "d")  "Error_State" "Error_State expected")
  (check-equal? (next-state "State0" "e")  "Error_State" "Error_State expected")
  (check-equal? (next-state "State0" "f")  "Error_State" "Error_State expected"))

#|
accepting-state? : State -> Boolean
GIVEN: a state of the machine
RETURNS: true iff the given state is a final (accepting) state

STRATEGY: Use of template for State
EXAMPLES:
(accepting-state "Accepting_State") = "True"
(accepting-state "State01") = "False"
(accepting-state "State02") = "False"
(accepting-state "State03") = "False"
(accepting-state "State04") = "False"
(accepting-state "Error_State") = "False"


|#
(define (accepting-state? state)
  (cond
    [ (equal? state "Accepting_State") #t]
    [else #f]))

;; TESTS
(begin-for-test
  (check-equal? (accepting-state? "Accepting_State") #t "True is the expected output")
  (check-equal? (accepting-state? "State1") #f "True is the expected output"))

#|
error-state? : State -> Boolean
GIVEN: a state of the machine
RETURNS: true iff there is no path (empty or non-empty) from the given
state to an accepting state

STRATEGY: Use of template for State

EXAMPLE:
(error-state "Error_State") = "True"
(error-state "State1") = "False"
(error-state "State2") = "False"
(error-state "State3") = "False"
(error-state "Accepting_State") = "False"


|#

(define (error-state? state)
  
(cond    [ (equal? state "Error_State") #t]
         [else #f]
 ))


;; TESTS
(begin-for-test
  (check-equal? (error-state? "Accepting_State") #f "True is the expected output")
  (check-equal? (error-state? "Error_State") #t "True is the expected output")
  (check-equal? (accepting-state?(next-state(next-state(next-state (initial-state 1) "q")"d")"e")) #t)
  (check-equal? (error-state?(next-state(next-state(next-state (initial-state 1) "q")"d")"u"))#t)
  (check-equal? (accepting-state?(next-state(next-state(next-state (initial-state 1) "q")"u")"u"))#f)
  (check-equal? (accepting-state?(next-state(next-state(next-state(next-state (initial-state 1) "q")"u")"u")"d"))#t))

