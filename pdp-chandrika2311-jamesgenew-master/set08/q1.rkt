;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

(provide
 make-def
 make-varexp
 make-appexp
 any-loops?
 )

;; A Program is a ListOfDefinition.
;; A ListOfDefinition is either
;; --empty
;; --(cons Definition ListOfDefinition)

;; Template:
;; ;; lod-fn : LOD -> ??
;; 
;; (define (lod-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (lod-fn (rest lst)))]))
;;____________________________

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.
;;Template:
;;def-fn: Definition -> ??
;;(define (def-fn d)
;;(...(def-name d)
;;(...(def-args d)
;;(...(def-body d)
;;____________________________
;; A Variable is a Symbol.
;;____________________________
;; A ListOfVariable is one of
;; --empty
;; (cons Variable ListOfVariable)

;; Template:
;; ;; lov-fn : LOV -> ??
;; (define (lov-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (lov-fn (rest lst)))]))
;;____________________________
(define-struct varexp (name))
(define-struct appexp (fn args))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; A ListOfExp is one of
;; --empty
;; --(cons Exp ListOfExp)
;; Template:
;; ;; loe-fn : LOE -> ??
;; (define (loe-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (loe-fn (rest lst)))]))
;;____________________________________________________________________________________________________
;;____________________________________________________________________________________________________
;;____________________________________________________________________________________________________
;;Test Cases:

(define some-loops
  (list
     (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
     (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
     (make-def 'f3 (list 'x 'u)
               (make-appexp 'f1 (list (make-appexp 'f4
                                             (list (make-varexp 'u)
                                                   (make-varexp 'w)))
                                      (make-varexp 'z))))
     (make-def 'f4 (list 'x 'y)
               (make-appexp 'f5
                            (list (make-varexp 'y)
                                  (make-varexp 'u))))
     (make-def 'f5 (list 'u)
               (make-appexp 'f2
                            (list (make-appexp 'f3 empty))))
     (make-def 'no-loop (list 'x) (make-varexp 'x))))

(define some-loops1
  (list
     (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
     (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
     (make-def 'f3 (list 'x 'u)
               (make-appexp 'f1 (list (make-appexp 'f4
                                             (list (make-varexp 'u)
                                                   (make-varexp 'w)))
                                      (make-varexp 'z))))
     (make-def 'f4 (list 'x 'y)
               (make-appexp 'f5
                            (list (make-varexp 'y)
                                  (make-varexp 'u))))
     (make-def 'no-loop (list 'x) (make-varexp 'x))))
(define some-loops3
  (list
     (make-def 'f1 (list 'x) (make-appexp 'f2 (list (make-varexp 'x))))
     (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
))
;;____________________________________________________________________________________________________
;;____________________________________________________________________________________________________
;;____________________________________________________________________________________________________
;; any-loops? : Program -> Boolean
;; GIVEN: a valid SGS program p 
;; RETURNS: true iff there is some function f in p that calls itself
;;          either directly or indirectly
;;EXAMPLES: (any-loops? some-loops)=>#t
;;          (any-loops? some-loops1)=>#f
;;STRETEGY: Use HOF Ormap on lod

(define (any-loops? lod)
  (ormap(lambda (def1)(are-there-any-loops? def1 lod)) lod))
(begin-for-test(check-equal?(any-loops? some-loops)#t))
(begin-for-test(check-equal?(any-loops? some-loops1)#f))
(begin-for-test(check-equal?(any-loops? some-loops3)#t))
;;____________________________________________________________________________________________________
;;are-there-any-loops? Definition Program
;;GIVEN: Definition
;;       Valid SGS Program
;;RETURN: True iff the definition is calling itself directly or indirectly
;;EXAMPLES:
#|(are-there-any-loops?(make-def 'f3 (list 'x 'u)
                                              (make-appexp 'f1
                                                           (list (make-appexp 'f4
                                                                              (list (make-varexp 'u)
                                                                                    (make-varexp 'w)))
                                                                 (make-varexp 'z)))) some-loops)=>#t|#
;;STRATEGY: Use Template for Def on d1
(define (are-there-any-loops? d1 p)
  (cond[(varexp? (def-body d1)) #false]
       [(appexp?(def-body d1)) (direct-indirect-call-of-def? (def-name d1) p)]))
;;;___________
(begin-for-test
  (check-equal?(are-there-any-loops?(make-def 'f3 (list 'x 'u)
                                              (make-appexp 'f1
                                                           (list (make-appexp 'f4
                                                                              (list (make-varexp 'u)
                                                                                    (make-varexp 'w)))
                                                                 (make-varexp 'z)))) some-loops)#t)
  (check-equal?(are-there-any-loops?(make-def 'f3 (list 'x 'u)
                                              (make-varexp 'f1)) some-loops)#f))
;;____________________________________________________________________________________________________
;;direct-indirect-call-of-def?: Def-Name Program
;;GIVEN: Def-name and Program
;;RETURNS: True iff definition with the same name calls it self directly or indirectly
;;EXAMPLES:(direct-indirect-call-of-def? 'f3 some-loops)=>#t
;(direct-indirect-call-of-def? 'f1 some-loops)=>#f
;;STRATEGY:Use Teamplate of Program on p
(define (direct-indirect-call-of-def? name p)
  (check-successors-body name (reachables1 empty (get-successors-from-program name p) p)))

;;;___________
(begin-for-test(check-equal?(direct-indirect-call-of-def? 'f3 some-loops)#t))
(begin-for-test(check-equal?(direct-indirect-call-of-def? 'f1 some-loops)#f))
(begin-for-test(check-equal?(direct-indirect-call-of-def? 'f3 some-loops1)#f))
;;____________________________________________________________________________________________________
;;check-successors-body: Definition-Name and ListOfVariables
;;GIVEN: Def-Name
;;RETURNS: True iff the definition calls itself directly or indirectly in the program
;;STRATEGY: Use HOD ormap on lor

(define (check-successors-body name lor)
 (ormap(lambda (n1)(equal? n1 name)) lor))
(begin-for-test(check-equal?(check-successors-body 'f3 (list 'f2 'no-loop 'f5 'f1 'f4 'f3))#t)) 
;;____________________________________________________________________________________________________
;;reachables1: SetOfVariable SetOfVariable Program -> SetOfVariable
;; GIVEN: two SetOfVariable and a finite program p
;; WHERE:
;;  reached is the set of variables reachable in graph g in fewer than n steps
;;        from a set of variables and
;;  recent is the set of variables reachable from S in n steps but
;;         not in n-1 steps.
;; RETURNS: the set of variables reachable from variables in p.
;;HALTING MEASURE: Number Of Variables in p that are not reached
(define(reachables1 reached recent p)
  (cond
    [(empty? recent) reached]
    [else
     ;;next-reached: SetOfVariable SetOfVariable Program
     ;;RETURNS: SetOfVariable
     (local
       ((define next-reached(append recent reached)) 
           (define next-recent
             (set-diff (check-each-defname recent p) next-reached)))
       (reachables1 next-reached next-recent p))]))
;;______________Tests:
(begin-for-test(check-equal?(reachables1 empty
                                         (get-successors-from-program 'f3 some-loops) some-loops)
                            (list 'f2 'f3 'no-loop 'f5 'f1 'f4))
               (check-equal?(reachables1 empty
                                         (get-successors-from-program 'f1 some-loops) some-loops1)
                            (list 'no-loop)))
;;____________________________________________________________________________________________________
;;check-each-defname:ListOfVariables  Program
;;GIVEN: ListOfVariables Program
;;RETURNS:List of successors of the Variables
;;HALTING MEASURE: Length of the list
;;TERMINATION REASONING:This function will always hault when the list-of-defnames is parsed completely
;; as at each iteration the list length is reducing
;;STRATEGY:Use template of ListOfVariables on List-of-defnames
(define(check-each-defname list-of-defnames p)
  (cond[(empty? list-of-defnames) empty]
       [else (append(get-successors-from-program (first list-of-defnames) p)
                    (check-each-defname(rest list-of-defnames) p))]))
;;______________Tests:
(begin-for-test(check-equal?(check-each-defname (list 'f3 'f5) some-loops)(list 'f1 'f4 'f2 'f3))
               (check-equal?(check-each-defname (list 'f1) some-loops)(list 'no-loop)))

;;____________________________________________________________________________________________________
;;get-successors-from-program: Variable Program
;;GIVEN: A variable and a program
;;RETURNS: List of the variable's successors
;;HALTING MEASURE: Length of Program
;;TERMINATION REASONING :The length of program will descrease on each iteration and hence this program
;;will always hault
;;STRATEGY:Use Template of Definition on p
(define(get-successors-from-program defname p)
  (cond[(empty? p) empty]
       [(equal? defname (def-name (first p)))(get-successor(def-body (first p)))]
       [else (get-successors-from-program defname (rest p))]))

;;______________Tests:
(begin-for-test(check-equal?(get-successors-from-program 'f3 some-loops) (list 'f1 'f4)))
(begin-for-test(check-equal?(get-successors-from-program 'f5 some-loops)(list 'f2 'f3)))
(begin-for-test(check-equal?(get-successors-from-program 'f5 empty) '()))
;;____________________________________________________________________________________________________
;;get-successor: Expression
;;GIVEN: Expression
;;RETURNS: List of Variables that are successors of the expression
;;STRATEGY:Use template of Expressions on e
(define(get-successor e)
  (cond[(varexp? e) empty]
       [(appexp? e) (cons (appexp-fn e) (get-successors-from-args-list(appexp-args e)))]
       ))
;;______________Tests:
(begin-for-test(check-equal?(get-successor (make-appexp 'f2
                                                        (list (make-appexp 'f3 empty))))
                            (list 'f2 'f3))
               (check-equal?(get-successor (make-varexp 'y))'()))
;;____________________________________________________________________________________________________
;;get-successors-from-args-list: ListOfExpression
;;GIVEN: ListOfExpression
;;RETURNS: ListOfVariable
;;HALTING MEASURE:Length of loe
;;TERMINATION REASONING: At each iteration the length of loe will desrease and hence this function
;will hault
;;STRATEGY: Use Template of expressions on loe
(define (get-successors-from-args-list loe)
   (cond[(empty? loe) empty]
        [(varexp? (first loe)) empty]
        [(appexp? (first loe))(cons (appexp-fn(first loe))
                                    (get-successors-from-args-list (rest loe)))]))
;;______________Tests:
 (begin-for-test(check-equal?(get-successors-from-args-list(list (make-appexp 'f4
                                                                        (list (make-varexp 'u)
                                                                              (make-varexp 'w)))
                                                           (make-varexp 'z)))(list 'f4)))
 (begin-for-test(check-equal?(get-successors-from-args-list(list (make-appexp 'f3 empty)
                                                                 (make-appexp 'f4 empty)))
                             (list 'f3'f4)))
