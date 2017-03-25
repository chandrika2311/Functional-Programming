;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;(require racket/list)
(require rackunit)
(require "extras.rkt")

(provide
 is-null-derivable?
 make-pos
 make-neg
 make-clause 
 )
;;______________________________________________________________________________________
;;References: DPLL algorithm reffered during design of this solution
;;______________________________________________________________________________________
#|
> (stress-benchmark2 5)
cpu time: 3 real time: 3 gc time: 0
#false
> (stress-benchmark2 10)
cpu time: 24 real time: 23 gc time: 0
#false
|#
;;______________________________________________________________________________________
;;______________________________________________________________________________________
;Data Definitions:
;;______________________________________________________________________________________
;;______________________________________________________________________________________
;; A Variable is a Racket Symbol.

(define-struct lit (name num))
;; A Literal is a (make-lit Variable Integer)
;; Interp : Variable field is the name of the variable, Integer field is -1 if negation, 1 otherwise
;; Template:
;; lit-fn: Literal -> ??
;; (define (lit-fn l)
;; (...(lit-name l)
;; (...(lit-num l)
;;_________________________________________________________________________________________________  
;;_________________________________________________________________________________________________  
;; A Clause is a SetOfLiteral
;; A SetOfLiteral is one of
;; --empty
;; --(cons Literal SetOfLiteral)
;; Template:
;; ;; lol-fn : LOL -> ??
;; (define (lol-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (lol-fn (rest lst)))]))
;;_________________________________________________________________________________________________
;; A ListOfClause is one of 
;;-- empty
;;-- (cons Clause ListOfClause)
;; Template:
;; ;; loc-fn : LOC -> ??
;; (define (loc-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (loc-fn (rest lst)))]))
;;_________________________________________________________________________________________________
;make-pos : Symbol -> Literal
;make-neg : Symbol -> Literal
;GIVEN: A Variable v
;RETURNS: The literal v or ¬v
;;STRATEGY: Use Teamplate for lit on s
(define (make-pos s)
  (make-lit s 1)
  )
(define (make-neg s)
  (make-lit s -1)
  )
(begin-for-test (check-equal? (make-pos 'z) (make-lit 'z 1))
                (check-equal? (make-neg 'f) (make-lit 'f -1)))

;;______________________________________________________________________________________
;; make-clause : ListOfLiteral -> Clause
;; GIVEN: a list of literals, possibly with duplications
;; RETURNS: a clause containing exactly those literals
;;STRATEGY:Combine simpler functions
(define (make-clause lol)
 lol)
(begin-for-test (check-equal? (make-clause (list (make-lit 'a 1) (make-lit 'b 1)
                                                 (make-lit 'c 1) (make-lit 'd 1))) example-lol))

(define example-lol (list (make-lit 'a 1) (make-lit 'b 1) (make-lit 'c 1) (make-lit 'd 1)))

;;______________________________________________________________________________________
;; first-unassigned-var : ListOfLiteral ListOfLiteral -> Literal
;; GIVEN: A listofliteral of assigned variable/truth values and a listofliteral
;;        representing all of the CNF's variables
;; WHERE: lov is not empty
;; RETURNS: A literal which has not been assigned yet
;; DESIGN STRATEGY: Divide into cases on (first lov) being a member of the assn
;;                  list
;;HALTING MEASURE: Length of lov
(define (first-unassigned-var assn lov)
  (cond
    [(member? (first lov) assn) (first-unassigned-var assn (rest lov))]
    [(member? (make-lit (lit-name (first lov)) (* (lit-num (first lov)) -1)) assn) 
     (first-unassigned-var assn (rest lov))]
    [else (first lov)]
    )
  )

(define example-assn (list (make-lit 'a 1) (make-lit 'b -1))) 
(define example-lov (list (make-lit 'a 1) (make-lit 'b 1) (make-lit 'c 1) (make-lit 'd 1)))
(begin-for-test (check-equal? (first-unassigned-var example-assn example-lov) (make-lit 'c 1)))
;;______________________________________________________________________________________
;; clause-true? : Literal ListOfLiteral -> Boolean
;; GIVEN: A literal representing a variable with a true/false assignment and a listofliteral
;;        representing a clause
;; RETURNS: True if the given clause is true with that variable assignment, false otherwise
;; DESIGN STRATEGY: Divide into cases on assn being a member of the list clause
(define (clause-true? assn clause)
  (cond
    [(empty? clause) #f]
    [(not (equal? (member assn clause) #f)) #t] 
    [else #f]
    )
  ) 
;;______________________________________________________________________________________
;; clause-true-top? : ListOfLiteral ListOfLiteral -> Boolean
;; GIVEN: A listofliteral representing assigned truth values and a ListOfLiteral
;;        representing a cnf clause
;; RETURNS: True if one of the truth values causes the clause to be true
;; DESIGN STRATEGY: Call simpler functions
;; HALTING CONDITION: assns is empty
(define (clause-true-top? assns clause)
  (cond
    [(empty? assns) #f]
    [else (or (clause-true? (first assns) clause) (clause-true-top? (rest assns) clause))]
    )
  )

;;______________________________________________________________________________________
;; cnf-true? ListOfClause ListOfLiteral -> Boolean
;; GIVEN: A listofclause representing a cnf and a literal representing a variable
;;        with a true/false assignment 
;; RETURNS: True if the CNF is true for the given variable with the given
;;          true/false assignment
;; DESIGN STRATEGY: Call HOF filter on cnf
(define (cnf-true? cnf assns)
  (equal? (filter (lambda (z) (clause-true-top? assns z)) cnf) cnf)
  )

(define cnfexample2 (list (list (make-lit 'a 1) (make-lit 'b -1)) (list (make-lit 'b 1))
                          (list (make-lit 'c -1) (make-lit 'd 1)) (list (make-lit 'e -1))))
(define assns2 (list (make-lit 'a 1) (make-lit 'b 1) (make-lit 'c -1)
                     (make-lit 'd 1) (make-lit 'e -1))) 
(begin-for-test (check-equal? (cnf-true? cnfexample2 assns2) #t))   
;;______________________________________________________________________________________
;; remove-dup : ListOfLiteral ListOfLiteral -> ListOfLiteral
;; GIVEN: A ListOfLiteral which may have duplicates and a ListOfLiteral which is
;;        storing the literals with no duplicates
;; RETURNS: A listofliteral with no duplicates
;; WHERE: newlol is initially empty
;;DESIGN STRATEGY: Use Template of Literal on lol
;; HALTING CONDITION: lol is empty
(define (remove-dup lol newlol)
  (cond
    [(empty? lol) newlol]
    [(equal? #f (member? (first lol) newlol)) (remove-dup (rest lol)
                                                          (append newlol (list (first lol))))]
    [else (remove-dup (rest lol) newlol)]
    )
  )
;;______________________________________________________________________________________
;; assemble-list-of-variables-top : ListOfClause -> ListOfLiteral
;; GIVEN: A ListOfClause
;; RETURNS: A ListOfLiteral representing every variable in the listofclause,
;;          with a true copy and a false copy for each
;; DESIGN STRATEGY: Call a more general function
(define (assemble-list-of-variables-top cnf)
  (remove-dup (assemble-list-of-variables cnf) empty)
  )

(define cnfexample (list (list (make-lit 'a 1) (make-lit 'b -1)) (list (make-lit 'b 1))
                         (list (make-lit 'c -1) (make-lit 'd 1))))
(begin-for-test (check-equal? (assemble-list-of-variables-top cnfexample)
                              (list (make-lit 'a 1) (make-lit 'b 1) (make-lit 'c 1) (make-lit 'd 1))))
;;______________________________________________________________________________________
;; assemble-list-of-variables : ListOfClause -> ListOfLiteral
;; GIVEN: A ListOfClause
;; RETURNS: A ListOfLiteral representing every variable in the listofclause,
;;          with a true copy and a false copy for each
;; DESIGN STRATEGY: Combine simpler functions
;; HALTING CONDITION: cnf empty
(define (assemble-list-of-variables cnf)
  (cond
    [(empty? cnf) empty]
    [else (append (assemble-list-of-variables-clause (first cnf))
                (assemble-list-of-variables (rest cnf)))]
    )
  )
;;______________________________________________________________________________________
;; assemble-list-of-variables-clause : Clause -> ListOfLiteral
;; GIVEN: A Clause
;; RETURNS: A list of all the variables in that clause
;; DESIGN STRATEGY: Divide into cases (first c) being false or true, add true
;;                  version to list
;; HALTING CONDITION: c is empty
(define (assemble-list-of-variables-clause c)
  (cond
    [(empty? c) empty]
    [(< (lit-num (first c)) 0) (cons (make-lit (lit-name (first c)) (* (lit-num (first c)) -1))
                                     (assemble-list-of-variables-clause (rest c)))]  
    [else (cons (first c) (assemble-list-of-variables-clause (rest c)))]
    )
  )
;;______________________________________________________________________________________
;; contained-in-assn? ListOfLiteral Literal -> Boolean
;; GIVEN: A ListOfLiteral representing variables and true/false assignments
;;        with no duplicates and a Literal
;; RETURNS: True if the literal is contained in the listofliteral in its true
;;          or false form
;; DESIGN STRATEGY: Divide into cases on v or its negation being equal to
;;                  (first lol)
;; HALTING CONDITION: lol is empty
(define (contained-in-assn? lol v)
  (cond
    [(empty? lol) #f]
    [(equal? (first lol) v) #t]
    [(equal? (first lol) (make-lit (lit-name v) (* (lit-num v) -1))) #t]
    [else (contained-in-assn? (rest lol) v)]
    )
  )

(define assn3 (list (make-lit 'a -1) (make-lit 'z 1) (make-lit 'b -1)))
(define testlit (make-lit 'a 1))
(define testlit2 (make-lit 'q -1))
(define testlit3 (make-lit 'z 1))
(begin-for-test (check-equal? (contained-in-assn? assn3 testlit) #t)
                (check-equal? (contained-in-assn? assn3 testlit2) #f)
                (check-equal? (contained-in-assn? assn3 testlit3) #t))
;;______________________________________________________________________________________
;; try-to-find-combo : ListOfLiteral ListOfClause -> Boolean
;; GIVEN: A ListOfLiteral representing the variables and true/false values
;;        attempted so far and a CNF
;; RETURNS: True if a combination is found which causes the CNF to be true
;; DESIGN STRATEGY: Use HOF filter on cnf
(define (try-to-find-combo assn cnf)
  (cond
     [(equal? (filter (lambda (x) (contained-in-assn? assn x))
                      (assemble-list-of-variables cnf))
              (assemble-list-of-variables cnf)) (cnf-true? cnf assn)]
     ;;check if all variables in cnf are in assn 
    [else (or (try-to-find-combo
               (append assn (list (first-unassigned-var assn (assemble-list-of-variables-top cnf))))
               cnf) 
              (try-to-find-combo
               (append assn
                       (list
                        (make-lit
                         (lit-name (first-unassigned-var assn(assemble-list-of-variables-top cnf)))
                         (* (lit-num
                             (first-unassigned-var assn(assemble-list-of-variables-top cnf)))-1))))
               cnf))]
    )
  ) 

(define cnfexample3 (list (list (make-lit 'a 1) (make-lit 'b -1)) (list (make-lit 'b 1))
                          (list (make-lit 'c -1) (make-lit 'd 1) (make-lit 'a -1))))
(define cnfexample4 (list (list (make-lit 'a 1) (make-lit 'b -1)) (list (make-lit 'b 1))
                          (list (make-lit 'b -1)) (list (make-lit 'c -1) (make-lit 'd 1)
                                                        (make-lit 'a -1))))
(begin-for-test (check-equal? (try-to-find-combo empty cnfexample3) #t)
                (check-equal? (try-to-find-combo empty cnfexample4) #f))   


;;______________________________________________________________________________________
;; is-cnf-empty? ListOfClause -> Boolean
;; GIVEN: A ListOfClause
;; RETURNS: True if the ListOfClause is empty
(define (is-cnf-empty? lol)
  (if (empty? lol) #t #f)
  )

;;______________________________________________________________________________________
;; empty-clause-exists? ListOfClause -> Boolean
;; GIVEN: A ListOfClause
;; RETURNS: True if any of the clauses are empty, indicating a CNF that cannot
;;          be satisfied
;; HALTING CONDITION: loc empty
;; DESIGN STRATEGY: Combine simpler functions
(define (empty-clause-exists? loc)
  (cond
    [(empty? loc) #f] 
    [(is-clause-empty? (first loc)) #t]
    [else (empty-clause-exists? (rest loc))]
    )
  )
;;______________________________________________________________________________________
;; is-clause-empty? ListOfLiteral -> Boolean
;; GIVEN: A ListOfLiteral
;; RETURNS: #t if the ListOfLiteral is empty, false otherwise
(define (is-clause-empty? c)
  (cond
    ;[(empty? (first c)) #t]
    [(empty? c) #t]
    [(empty? (first c)) #t]
    [else #f]
    )
  )
;;______________________________________________________________________________________
;; is-null-derivable? : ListOfClause -> Boolean
;; GIVEN: a list of clauses
;; RETURNS: true iff the empty clause is derivable from the given
;;          clauses.
;; DESIGN STRATEGY: Combine simpler functions
(define (is-null-derivable? cnf)
  (cond
    [(is-cnf-empty? cnf) #f] ;; an empty cnf returns false
    [(empty-clause-exists? cnf) #t]
    [else (not (try-to-find-combo empty cnf))]
    )
  )
;;______________________________________________________________________________________
;;______________________________________________________________________________________
;;                                   TEST CASES
;;______________________________________________________________________________________
;;______________________________________________________________________________________
;;______________________________________________________________________________________

(begin-for-test (check-equal? (is-null-derivable? examplecnf) #t)
                (check-equal? (is-null-derivable? examplecnf2) #t)
                (check-equal? (is-null-derivable? examplecnf3) #f)
                (check-equal? (is-null-derivable? empty) #f)
                (check-equal? (is-null-derivable? examplecnf5) #t)
                (check-equal? (is-null-derivable? loc6) #f))

(define examplecnf (list (list (make-lit 'a 1) (make-lit 'b -1) (make-lit 'c 1))
                         ;;example from assignment page
                         (list (make-lit 'd 1) (make-lit 'b 1))
                         (list (make-lit 'a -1) (make-lit 'c 1))
                         (list (make-lit 'b 1))
                         (list (make-lit 'c -1)))) 
(define examplecnf2 (list (list (make-lit 'a 1) (make-lit 'b -1) (make-lit 'c 1)) (list '())))
(define examplecnf3 (list (list (make-lit 'a 1) (make-lit 'b -1) (make-lit 'c 1))
                          (list (make-lit 'c -1) (make-lit 'd -1)) (list (make-lit 'z -1)))) 
(define examplecnf4 (list (list (make-lit 'a 1) (make-lit 'b -1) (make-lit 'c 1)) '()))

(define examplecnf5
  (list
   (make-clause (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))
   (make-clause (list (make-neg 'c) (make-pos 'd)))
   (make-clause '())))

(define loc6 '())

                                                

(define loc2
  (list
   (make-clause (list (make-pos 'a) (make-pos 'b)))
   (make-clause (list (make-neg 'a) (make-neg 'b)))))



(define loc4
  (list
   (make-clause (list (make-pos 'c)))
   (make-clause (list (make-neg 'c)))))

(define loc5
  (list
   (make-clause (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))
   (make-clause (list (make-neg 'c) (make-pos 'd)))
   (make-clause (list (make-pos 'd)))))

(define loc8
  (list
   (make-clause (list (make-pos 'p1)
                      (make-neg 'p2)
                      (make-neg 'p3)))
  (make-clause (list (make-neg 'p1)
                     (make-pos 'p2)
                     (make-neg 'p3)))
  (make-clause (list (make-neg 'p1)
                     (make-neg 'p2)
                     (make-pos 'p3)))))
(define loc3
  (list
   (make-clause (list (make-pos 'a)(make-pos 'b)(make-pos 'c)))
   (make-clause (list (make-neg 'a)))
   (make-clause (list (make-neg 'b)))
   (make-clause (list (make-neg 'c)))))
(define loc7 (list(make-clause (list (make-pos 'd)))))


(define loc9
  (list
   (make-clause (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))
   (make-clause (list (make-neg 'c) (make-pos 'd)))
   (make-clause '())))
(define loc10
   (list (make-clause (list (make-neg 'c)))
    (make-clause (list (make-neg 'b)))
    (make-clause (list (make-pos 'a)
                       (make-pos 'c)
                       (make-pos 'b)))
    (make-clause (list (make-neg 'a)))
    (make-clause (list (make-pos 'c)
                       (make-pos 'b)))))
(define loc12
  (list
   (make-clause (list (make-neg 'b) (make-pos 'c)))
   (make-clause (list (make-pos 'a) (make-pos 'c)))
   (make-clause (list (make-neg 'a) (make-pos 'b)))
   (make-clause (list (make-neg 'a) (make-neg 'b)))
   (make-clause (list (make-pos 'a) (make-neg 'b)))
   (make-clause (list (make-pos 'b) (make-neg 'c)))))
(define loc16
  (list
   (make-clause (list (make-pos 'a) (make-neg 'b)))
   (make-clause  (list (make-neg 'a) (make-pos 'b)))
   (make-clause (list (make-neg 'b)))
   (make-clause (list (make-pos 'b)))))
(define loc11
  (list
   (make-clause (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))
   (make-clause (list (make-neg 'a) (make-neg 'b) (make-pos 'd)))
   (make-clause (list (make-neg 'd)))))


(define loc1
  (list
   (make-clause (list (make-pos 'a) (make-neg 'b) (make-pos 'c)))
   (make-clause (list (make-pos 'd) (make-pos 'b)))
   (make-clause (list (make-neg 'a) (make-pos 'c)))
   (make-clause (list (make-pos 'b)))
   (make-clause (list (make-neg 'c)))))

(define loc13
  (list
   (make-clause  (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))
   (make-clause (list (make-neg 'a) (make-neg 'b)))
   (make-clause (list (make-pos 'b) (make-neg 'c)))
   (make-clause (list (make-neg 'b) (make-neg 'c)))
   (make-clause (list (make-neg 'a) (make-pos 'c)))
   (make-clause (list (make-neg 'b) (make-pos 'c)))))

(define loc14
  (list
   (make-clause (list (make-pos 'a) (make-neg 'b) (make-pos 'c)))
   (make-clause (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))))

(define loc15
  (list
   (make-clause (list (make-pos 'a) (make-neg 'a)))))




(begin-for-test
  (check-equal? (is-null-derivable? loc1) #true "should derive null")
  (check-equal? (is-null-derivable? loc2) #false "should not derive null")
  (check-equal? (is-null-derivable? loc3) #true "should derive null")
  (check-equal? (is-null-derivable? loc4) #true "should derive null")
  (check-equal? (is-null-derivable? loc5) #false "should not derive null")
  (check-equal? (is-null-derivable? loc6) #false "should not derive null")
  (check-equal? (is-null-derivable? loc7) #false "should not derive null")
  (check-equal? (is-null-derivable? loc8) #false "should not derive null")
  (check-equal? (is-null-derivable? loc9) #true "should derive null")
  (check-equal? (is-null-derivable? loc10) #true "should derive null")
  (check-equal? (is-null-derivable? loc11) #false "should not derive null")
  (check-equal? (is-null-derivable? loc12) #true "should derive null")
  (check-equal? (is-null-derivable? loc13) #true "should derive null")
  (check-equal? (is-null-derivable? loc14) #false "should not derive null")
  (check-equal? (is-null-derivable? loc16) #true "should derive null"))
