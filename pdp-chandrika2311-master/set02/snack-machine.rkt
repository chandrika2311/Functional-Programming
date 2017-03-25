;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snack-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide initial-machine
         machine-next-state
         machine-output
         machine-remaining-kale
         machine-remaining-carrots
         machine-bank)
#|
Purpose: To design a vending machine which accepts money and choice as customer input and returns an output.
 
;DATA-DEFINITION:
A CustomerInput is one of
-- a PosInt        interp: insert the specified number of quarters
-- "kale"          interp: request a bag of kale chips
-- "carrots"       interp: request a bag of carrots
-- "change"        interp: return all the unspent money that the  customer has inserted
TEMPLATE
CustomerInput-fn : custInput -> ??
(define (CustomerInput-fn customerInput)
 (cond
   [(string=? CustomerInput "kale")    ...]
   [(string=? CustomerInput "carrots") ...]
   [(string=? CustomerInput "change")  ...]
   [(else )...]))  

A MachineOutput is one of
-- "kale"           interp: machine dispenses a bag of kale chips
-- "carrots"        interp: machine dispenses a bag of carrot sticks
-- "Out of Item"    interp: machine displays "Out of Item"
-- a PosInt         interp: machine releases the specified number of quarters
-- "Nothing"        interp: the machine does nothing

TEMPLATE:
machineOutput-fn : machineOutput -> ??
(define (machineOutput-fn machineOutput)
 (cond
   [... ("kale")]
   [...("carrots")]
   [...("Out of Item")]
   [...(a PosInt)]
   [else("Nothing")]
   ))  
|#


(define-struct machineState (kaleNum carrotNum bankMoney bufferMoney))
#|
A MachineState is a

   (make-machineState NonNegInt NonNegInt NonNegInt NonNegInt)

Interpretation:

 kaleNum is the number of kale chips packets, NonNegInt
 carrotNum is the number of Carrot sticks, NonNegInt
 bankMoney is the money in the bank, NonNegInt
 bufferMoney is change money in the buffer, NonNegInt
                   
(define (machineState-fn m)
  (...
    (machineState-kaleNum m)
    (machineState-carrotnum m)
    (machineState-bankmoney m)
    (machineState-buffermoney m)
    ))))
|#
#|
;;initial-machine : NonNegInt NonNegInt -> MachineState
;;GIVEN: a number of bags of kale chips and carrot sticks
;;RETURNS: the state of a machine loaded with the given numbers of bags
;;of kale chips and carrot sticks, with an empty bank.
EXAMPLE:
(initial-machine 4 5) =>(make-machineState 3 4 0 0)
(initial-machine 1 2) =>(make-machineState 1 2 0 0)
(initial-machine 0 0) =>(make-machineState 0 0 0 0)
|#
;;STRATEGY: Combining simpler functions

(define (initial-machine kaleNum carrotNum)
  
  ( make-machineState kaleNum carrotNum 0 0)
    )
(begin-for-test
  (check-equal? (initial-machine 3 4) (make-machineState 3 4 0 0)))

#|
machine-next-state : MachineState CustomerInput -> MachineState
GIVEN: a machine state and a customer input
RETURNS: the state of the machine that should follow the customer's input
EXAMPLE:
(machine-next-state (initial-machine 3 4) 5)=>(make-machineState 3 4 0 5)
(machine-next-state(machine-next-state (initial-machine 3 4) 5)"carrots")=>(make-machineState 3 2 4 1)
(machine-next-state(machine-next-state (initial-machine 3 4) 5)"kale")=>(make-machineState 2 4 3 2)
(machine-next-state(machine-next-state (initial-machine 3 4) 5)"change")=>(make-machineState 3 4 0 5)
|#
;;STRATEGY: cases on customerInput 
(define (machine-next-state m c)
  (cond [(string? c) (cond[(string=? c "kale") (kaleInput m)]
                          [(string=? c "carrots")(carrotInput m)]
                          [(string=? c "change") m])]

        [(integer? c) (money_inputed m c)]))

(begin-for-test
  (check-equal? (machine-next-state(machine-next-state(initial-machine 3 4) 6) "kale") (make-machineState 2 4 3 3))
  (check-equal? (machine-next-state(machine-next-state(initial-machine 3 4) 6) "carrots") (make-machineState 3 3 2 4))
  (check-equal? (machine-next-state(machine-next-state(machine-next-state(initial-machine 3 4) 10) "carrots")"change") (make-machineState 3 3 2 8))

  )
  

;Helper functions:
;;money-inputed: machineState posInt -> machineState
;;GIVEN: a machineState and amount of cash inputed
;;RETURNS: Machinestate with bufferMoney in machine state changed
;;EXAMPLE:
;;(money_inputed (initial-machine 3 4) 1)=>(make-machineState 3 4 0 1)
;;(money_inputed (initial-machine 3 4) 4)=>(make-machineState 3 4 0 4)
;;STRATEGY: Combining simpler functions

(define (money_inputed m cash)
  (make-machineState (machineState-kaleNum m) (machineState-carrotNum m) (machineState-bankMoney m) cash))
(begin-for-test
  (check-equal? (money_inputed (initial-machine 3 4) 1)(make-machineState 3 4 0 1))
  (check-equal?(money_inputed (initial-machine 3 4) 4) (make-machineState 3 4 0 4)))

;;---------------------------------------------------------------------------------
;;kaleInput: machineState -> machineState
;;GIVEN: a machineState
;;RETURNS: machineState after calculation on number of kale left and bankmoney bufferMoney
;;EXAMPLE:
;;(kaleInput (make-machineState 3 4 2 4)) => (make-machineState 3 4 2 1)
;;(kaleInput m) => (make-machineState 3 4 1 2)
;;STRATEGY: Combining simpler functions

(define (kaleInput m)
  (cond[(>=(machineState-bufferMoney m) 3)(make-machineState (- (machineState-kaleNum m) 1 )
                                                            (machineState-carrotNum m)
                                                            (+ (machineState-bankMoney m) 3 )
                                                            (-(machineState-bufferMoney m) 3))]
       [else m]))
(begin-for-test 
  (check-equal? (kaleInput(make-machineState 3 4 2 4)) (make-machineState 2 4 5 1)))

;;---------------------------------------------------------------------------------
;;carrotInput: machineState -> machineState
;;GIVEN: a machineState
;;RETURNS: machineState after calculation on number of carrot left and bankmoney bufferMoney
;;EXAMPLE:
;;(carrotInput m) => (make-machineState 3 4 2 1)
;;(carrotInput m) => (make-machineState 3 4 1 2)
;;STRATEGY: cases on machineState
(define (carrotInput m)
  (cond[(>=(machineState-bufferMoney m) 2)(make-machineState ( machineState-kaleNum m)
                                                            (-(machineState-carrotNum m) 1)
                                                            (+ (machineState-bankMoney m) 2)
                                                            (-(machineState-bufferMoney m) 2))]
                           [else m]))

(begin-for-test 
  (check-equal? (carrotInput(make-machineState 3 4 2 4)) (make-machineState 3 3 4 2)))
;;---------------------------------------------------------------------------------  
;;machine-output : MachineState CustomerInput -> MachineOutput
;;GIVEN: a machine state and a customer input
;;RETURNS: a MachineOutput that describes the machine's response to the
;;customer input
;;EXAMPLES:
;;(machineOutput(machine-next-state(machine-next-state (initial-machine 3 4) 5)"kale")"change")=> 2
;;(machineOutput(machine-output(machine-next-state(machine-next-state(machine-next-state (initial-machine 3 4) 4)"carrots")"carrots") "change") =>"nothing"
;;STRATEGY: cases on customerInput
(define (machine-output m c)
 (cond
   [(and (string=? c "kale")(not(zero? (machineState-kaleNum m)))) "kale"]
   [(and (string=? c "carrots")(not(zero? (machineState-carrotNum m)))) "carrots"]
   [(and (string=? c "kale")(zero? (machineState-kaleNum m))) "Out of Item"]
   [(and (string=? c "carrots")(zero? (machineState-carrotNum m))) "Out of Item"]
   [(string=? c "change") (cond[(= (machineState-bufferMoney m) 0) "nothing"]
                               [else (machineState-bufferMoney m)])]
   [else "nothing" ]
   ))

  (begin-for-test
   
   (check-equal?(machine-output(machine-next-state(machine-next-state (initial-machine 3 4) 5) "kale") "change") 2)
  (check-equal? (machine-output(machine-next-state(machine-next-state(machine-next-state (initial-machine 3 4) 4)"carrots")"carrots") "change") "nothing"))
;;---------------------------------------------------------------------------------  

;;machine-remaining-kale : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the number of bags of kale chips left in the machine
;;EXAMPLES:
;;(machine-remaining-kale(machine-next-state(machine-next-state (initial-machine 3 4) 5)"carrots")) => 3

;;;;STRATEGY: Combining simpler functions
(define (machine-remaining-kale m)
  (machineState-kaleNum m))
  (begin-for-test
   
   (check-equal? (machine-remaining-kale(machine-next-state(machine-next-state (initial-machine 3 4) 5)"carrots")) 3))
;;---------------------------------------------------------------------------------  

;;machine-remaining-carrots : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the number of bags of carrots left in the machine
;;EXAMPLES:
;;(machine-remaining-carrots(machine-next-state(machine-next-state (initial-machine 3 4) 5)"carrots")) => 3

;;STRATEGY: Combining simpler functions

(define (machine-remaining-carrots m)
 (machineState-carrotNum m))

(begin-for-test
   (check-equal? (machine-remaining-carrots(machine-next-state(machine-next-state (initial-machine 3 4) 5)"carrots")) 3))

;;---------------------------------------------------------------------------------  

;;machine-bank : MachineState -> NonNegInt
;;GIVEN: a machine state
;;RETURNS: the amount of money in the machine's bank, in cents
;;EXAMPLES:
;;(machine-bank(machine-next-state(machine-next-state (initial-machine 3 4) 5)"carrots")) => 50
;;(machine-bank(machine-next-state(machine-next-state (initial-machine 3 4) 5)"kale")) => 
;;STRATEGY: Combining simpler functions
(define (machine-bank m)
 (* 25 (machineState-bankMoney m)))

(begin-for-test
   (check-equal? (machine-bank(machine-next-state(machine-next-state (initial-machine 3 4) 5)"carrots")) 50)
   (check-equal? (machine-bank(machine-next-state(machine-next-state (initial-machine 3 4) 5)"kale")) 75))

;;---------------------------------------------------------------------------------  