;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide felleisen-roster
         shivers-roster
         possible-roster?
         acceptable-felleisen-answer?
         make-slip
         slip-color
         slip-name1
         slip-name2)

#|
###################################################################################################
                                       DEFINITIONS:
###################################################################################################

List of Slips:
------------------
 A ListOfSlips(LOS) is one of: 
 --empty
 --(cons Slips LOS)
 los-fn: LOS->??
  (define (los-fn lod)
   (cond
     [(empty? los)...]
     [else (...
             (slip-fn (first lod))
             (loc-fn (rest lod)))]))
A Color is one of
-- FELLEISEN-COLOR
-- SHIVER-COLOR
TEMPLATE:

 color-fn : Color -> ??
;(define (color-fn c)
;  (cond
;    [(string=? c FELLEISEN-COLOR)    
;     ...]
;    [(string=? c SHIVER-COLOR)
;     ...]))    

|#

(define-struct slip (color name1 name2))
;; A Slip is a (make-slip String String color)
;; Interpretation: 
;; name1: First name String
;; name2: Last name Srting
;;Color: accepts only FELLEISEN-COLOR or SHIVER-COLOR
;; TEMPLATE
;; slip-fn : Slip -> ??
;;(define (slip-fn s)
;;  (... (slip-color s) (slip-name1 s)(slip-name2 s))

;##################################################################################################
;...........................................CONSTANTS..............................................
;##################################################################################################
(define FELLEISEN-COLOR "yellow")
(define SHIVER-COLOR "blue")


;###################################################################################################
;...........................................Test Cases.............................................
;###################################################################################################
(define yellow-list-test1(list
                          (make-slip FELLEISEN-COLOR "Wang" "Xi")
                          (make-slip FELLEISEN-COLOR "Jones" "Tom")
                          (make-slip FELLEISEN-COLOR "Tom" "Jone")
                          (make-slip FELLEISEN-COLOR "Xi" "Wang")
                          (make-slip FELLEISEN-COLOR "Shriram" "K.")))
(define mixed-list-test4 (list
                          (make-slip FELLEISEN-COLOR "Wang" "Xi")
                          (make-slip SHIVER-COLOR "Jones" "Tom")
                          (make-slip SHIVER-COLOR "Tom" "Jone")
                          (make-slip FELLEISEN-COLOR "Xi" "Wang")
                          (make-slip FELLEISEN-COLOR "Shriram" "K.")))
(define shiver-list-test1(list
                          (make-slip SHIVER-COLOR "Jones" "Tom")
                          (make-slip SHIVER-COLOR "Tom" "Jone")))
(define yellow-list-test2 (list
                           (make-slip FELLEISEN-COLOR "Wang" "Xi")
                           (make-slip FELLEISEN-COLOR "Shriram" "K.")))
(define yellow-list-test3 (list
                           (make-slip FELLEISEN-COLOR "Jones" "Tom")
                           (make-slip FELLEISEN-COLOR "Tom" "Jone")))

(define mixed-list-test6 (list
                          (make-slip FELLEISEN-COLOR "Jones" "Tom")
                          (make-slip FELLEISEN-COLOR "Tom" "Jones")
                          (make-slip FELLEISEN-COLOR "Tom" "Jone")
                          (make-slip SHIVER-COLOR "Xi" "Wang")))
(define yellow-list-test7 (list
                           (make-slip FELLEISEN-COLOR "Jones" "Tom")
                           (make-slip FELLEISEN-COLOR "Tom" "Jone")
                           (make-slip FELLEISEN-COLOR "chand" "shar")
                           (make-slip FELLEISEN-COLOR "shar" "chand")
                           ))
(define yellow-list-test10 (list
                            (make-slip FELLEISEN-COLOR "Jones" "Tom")
                            (make-slip FELLEISEN-COLOR "Tom" "Jone")
                            (make-slip FELLEISEN-COLOR "Shriram" "K.")))
(define list-test11 (list
                     (make-slip FELLEISEN-COLOR "Jones" "Tom")
                     (make-slip FELLEISEN-COLOR "Wang" "Xi")
                     (make-slip FELLEISEN-COLOR "Tom" "Jone")
                     (make-slip FELLEISEN-COLOR "Xi" "Wang")
                     (make-slip FELLEISEN-COLOR "Shriram" "K.")))
(define yellow-list-test13 (list
                            (make-slip FELLEISEN-COLOR "Jones" "Tom")
                            (make-slip FELLEISEN-COLOR "Tom" "Jone")
                            (make-slip FELLEISEN-COLOR "Shriram" "K.")))
(define yellow-list-test14 (list
                            (make-slip FELLEISEN-COLOR "Wang" "Xi")
                            (make-slip FELLEISEN-COLOR "Jones" "Tom")
                            (make-slip FELLEISEN-COLOR "Tom" "Jone")
                            (make-slip FELLEISEN-COLOR "Xi" "Wang")))
(define yellow-list-test15 (list
                            (make-slip FELLEISEN-COLOR "Wang" "Xi")))
(define yellow-list-test16 (list
                            (make-slip FELLEISEN-COLOR "Jones" "Tom")
                            (make-slip FELLEISEN-COLOR "Wang" "Xi")
                            (make-slip FELLEISEN-COLOR "Tom" "Jone")
                            (make-slip FELLEISEN-COLOR "Xi" "Wang")))

;__________________________________________@__________________________________________
#|
no-duplicate? : s LOS -> Boolean
GIVEN: a list of slips and a slip
RETURNS: returns true if no duplicate in the entire list los for s
EXAMPLES:(no-duplicate? s los) => #f
STRATEGY: Use template for LOS on lst
|#
(define (no-duplicate? s los);; returns true if no duplicate in the entire list los for s
  ;;GIVEN: slip
  ;;RETURNS:true iff slip is equal to s
  (andmap (lambda (s1) (not (check-mismatch-equal? s s1))) los))
;-----Tests:
(begin-for-test
  (check-equal? (no-duplicate? (first yellow-list-test1) yellow-list-test1) #f))
;__________________________________________@__________________________________________
#|
check-mismatch-equal? : s LOS -> Boolean
GIVEN: 2 slips 
RETURNS: returns true if they are equal
EXAMPLES:(check-mismatch-equal? (make-slip FELLEISEN-COLOR "Jones" "Tom")
(make-slip FELLEISEN-COLOR "Shriram" "K.")) => #f
STRATEGY: Combine simpler functions
|#                                                             
(define (check-mismatch-equal? s1 s2)
  (and(string=? (slip-name1 s1) (slip-name2 s2))
      (string=? (slip-name2 s1) (slip-name1 s2))
      (string=? (slip-color s1) (slip-color s2))))
;-----Tests:
(begin-for-test
  (check-equal? (check-mismatch-equal? (make-slip FELLEISEN-COLOR "Jones" "Tom")
                                       (make-slip FELLEISEN-COLOR "Shriram" "K.")) #f) )
;__________________________________________@__________________________________________
#|
felleisen-roster : ListOfSlip -> ListOfSlip
GIVEN: a list of slips
RETURNS: a list of slips containing all the students in Professor
Felleisen's class, without duplication.
EXAMPLES:(felleisen-roster yellow-list-test1)=> yellow-list-test2
STRATEGY: Use template for LOS on lst
|#
(define (felleisen-roster los)
  (return-roster los "felleisen"))
;-----Tests:
(begin-for-test
  (check-equal? (felleisen-roster yellow-list-test1)
                (list (make-slip FELLEISEN-COLOR "Jones" "Tom")
                      (make-slip FELLEISEN-COLOR "Tom" "Jone")
                      (make-slip FELLEISEN-COLOR "Xi" "Wang")
                      (make-slip FELLEISEN-COLOR "Shriram" "K.")))
  (check-equal? (felleisen-roster mixed-list-test4)
                (list  (make-slip FELLEISEN-COLOR "Xi" "Wang")
                       (make-slip FELLEISEN-COLOR "Shriram" "K."))))
;__________________________________________@__________________________________________
#|
return-roster : ListOfSlip Teacher-> list of slips
GIVEN: a list of slips and teacher name
RETURNS: a list of slips containing all the students for specified teacher
EXAMPLES:(return-roster mixed-list-test4 "shivers")=>shiver-list-test1
STRATEGY: Use template for LOS on lst
|#
(define (return-roster los teacher)
  (cond[(string=? teacher "felleisen")
        ;;GIVEN: Slip
        ;RETURNS: true iff color = FELLEISEN COLOR and No Duplicate occurance of it in los 
        (filter(lambda (s1)(and(string=? FELLEISEN-COLOR (slip-color s1))
                               (no-duplicate? s1 (rest los)))) los)]
       [(string=? teacher "shiver")
        ;;GIVEN: Slip
        ;RETURNS: true iff color = SHIVER-COLOR and No Duplicate occurance of it in los
        (filter(lambda (s1)(and(string=? SHIVER-COLOR (slip-color s1))
                               (no-duplicate? s1 (rest los)))) los)]))
;-----Tests:
(begin-for-test(check-equal?(return-roster (list (make-slip FELLEISEN-COLOR "Jones" "Tom")
                                                 (make-slip FELLEISEN-COLOR "Tom" "Jone")
                                                 (make-slip FELLEISEN-COLOR "Xi" "Wang")
                                                 (make-slip SHIVER-COLOR "Shriram" "K."))"shiver")
                            (list (make-slip SHIVER-COLOR "Shriram" "K.")))
               (check-equal? (return-roster mixed-list-test4 "shiver")shiver-list-test1))
;__________________________________________@__________________________________________
#|
shivers-roster: ListOfSlip -> ListOfSlip
GIVEN: a list of slips
RETURNS: a list of slips containing all the students in Professor
         Shivers' class, without duplication.
EXAMPLES:(shivers-roster mixed-list-test4)=>(list (make-slip SHIVER-COLOR "Jones" "Tom")
 (make-slip SHIVER-COLOR "Tom" "Jone")) ))
STRATEGY: Combine Simpler Functions
|#

(define (shivers-roster los)
  (return-roster los "shiver") )
;-----Tests:
(begin-for-test(check-equal? (shivers-roster mixed-list-test4)
                             (list (make-slip SHIVER-COLOR "Jones" "Tom")
                                   (make-slip SHIVER-COLOR "Tom" "Jone")) )
               (check-equal? (shivers-roster yellow-list-test3) empty )
               (check-equal? (shivers-roster yellow-list-test3) empty ))
;__________________________________________@__________________________________________
;;check-slip-color: s los => Boolean
;;GIVEN : a slip and a list of slips
;;RETURNS: true iff colors in the list are same
;;STRATEGY: Use HOF Andmap on los
(define (check-slip-color s los)
  ;;GIVEN: Slip
  ;;RETURNS: true iff color of slip same as color os s
  (andmap(lambda (s1) (color-check? s s1)) los))
;-----Tests:
(begin-for-test
  (check-equal? (check-slip-color (first yellow-list-test3) (rest yellow-list-test3))
                #t)
  (check-equal? (check-slip-color (first mixed-list-test4) (rest mixed-list-test4))
                #f))
;__________________________________________@__________________________________________
;;colorcheck?: slip1 slip2
;;GIVEN: S1 S2
;;RETURN: true iff color of both slips is same
;;examples: (color-check?(make-slip FELLEISEN-COLOR "Wang" "Xi")
;;(make-slip FELLEISEN-COLOR "Wang" "Xi"))=> #t
(define (color-check? s1 s2)
  ( string=? (slip-color s1) (slip-color s2) ))
;-----Tests:
(begin-for-test
  (check-equal?(color-check?(make-slip FELLEISEN-COLOR "Wang" "Xi")
                            (make-slip FELLEISEN-COLOR "Wang" "Xi"))#t))
;__________________________________________@__________________________________________
#|
possible-roster? : ListOfSlip -> Boolean
GIVEN: a list of slips
RETURNS: true iff all the slips in the list are the same color,
                  and no student is represented twice.
EXAMPLES: (possible-roster? yellow-list-test3)=> #t
STRATEGY: Use HOF Andmap on los
|#

(define (possible-roster? los)
  ;;GIVEN: Slip
  ;;RETURNS: True iff color of s and s1 same and no duplicates of s1 in los
  (andmap(lambda(s1)(and(check-slip-color s1 (rest los))(no-duplicate? s1 (rest los)))) los))
;-----Tests:
(begin-for-test
  (check-equal? (possible-roster? yellow-list-test1 ) #f)
  (check-equal? (possible-roster? yellow-list-test2 ) #t)
  (check-equal? (possible-roster?  mixed-list-test4)  #f))
;__________________________________________@__________________________________________
#|
acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
GIVEN: two lists of slips, lst1 and lst2
RETURNS: true iff every student on a yellow slip in lst1 appears once
and only once in lst2.
EXAMPLES:(acceptable-felleisen-answer? yellow-list-test1  empty ) => #f
STRATEGY: Use template for LOS on lst
|#

(define (acceptable-felleisen-answer? lst1 lst2 )
  (calling-comparision-list? (felleisen-roster(get-felleisen lst1))  lst2))
;-----Tests:
(begin-for-test(check-equal? (acceptable-felleisen-answer? yellow-list-test10 list-test11) #t)
               (check-equal? (acceptable-felleisen-answer? yellow-list-test15 yellow-list-test16) #f)
               (check-equal? (acceptable-felleisen-answer? yellow-list-test13 yellow-list-test14) #f)
               (check-equal? (acceptable-felleisen-answer? yellow-list-test1 yellow-list-test3) #f)
               (check-equal?(acceptable-felleisen-answer? mixed-list-test6 yellow-list-test7) #t))

;__________________________________________@__________________________________________
#|
get-felleisen: los -> los
GIVEN: LIST OF SLIPS
RETURNS: List Of Slips
EXAMPLES:
(get-felleisen-from1 yellow-list-test1)=> (list
 (make-slip FELLEISEN-COLOR "Wang" "Xi")
 (make-slip FELLEISEN-COLOR "Jones" "Tom")
 (make-slip FELLEISEN-COLOR "Tom" "Jone")
 (make-slip FELLEISEN-COLOR "Xi" "Wang")
 (make-slip FELLEISEN-COLOR "Shriram" "K."))))
STRATEGY: Use HOF filter on los
|#
(define (get-felleisen los)
  ;;GIVEN: slip
  ;;RETURNS: true iff slip is yellow
  (filter (lambda (s1)(string=? FELLEISEN-COLOR (slip-color s1) )) los))
;-----Tests:
(begin-for-test
  (check-equal? (get-felleisen yellow-list-test1) (list
                                                   (make-slip FELLEISEN-COLOR "Wang" "Xi")
                                                   (make-slip FELLEISEN-COLOR "Jones" "Tom")
                                                   (make-slip FELLEISEN-COLOR "Tom" "Jone")
                                                   (make-slip FELLEISEN-COLOR "Xi" "Wang")
                                                   (make-slip FELLEISEN-COLOR "Shriram" "K."))))

;__________________________________________@__________________________________________
#|
calling-comparision-list?:los los -> Boolean
GIVEN: 2 LISTs OF SLIPS
RETURNS:Boolean
EXAMPLES:
(calling-comparision-list? yellow-list-test10 list-test11)=>#t
STRATEGY: Use HOF andmap on lst1
|#
(define (calling-comparision-list? lst1 lst2)
  ;;GIVEN: slip
  ;;RETURNS:true iff slip found in lst2 
  (andmap(lambda (s1) (compare-lists? s1 lst2)) lst1))
;-----Tests:
(begin-for-test
  (check-equal? (calling-comparision-list? yellow-list-test10 list-test11) #t)
  (check-equal? (calling-comparision-list? yellow-list-test1 yellow-list-test3) #f))
;__________________________________________@__________________________________________
#|
compare-lists?:slip los -> Boolean
GIVEN: LIST OF SLIPS
RETURNS: true iff the slip occurs only once in the list of slips
EXAMPLES:
(compare-lists? (first yellow-list-test10)  list-test11)=> #t
(compare-lists? (make-slip FELLEISEN-COLOR "chan" "sha")  list-test11)=> #f
STRATEGY: Use template for LOS on lst
HALTING MEASURE: Length Of List

|#
(define (compare-lists? s lst2)
  (cond[(empty? lst2) #f]
       [(or(equal? s (first lst2)) (check-mismatch-equal? s (first lst2)))
        (not (compare-deeper-list2? s (rest lst2)))]
       [else (compare-lists? s (rest lst2))]))
;-----Tests:
(begin-for-test
  (check-equal?(compare-lists? (first yellow-list-test10)  list-test11) #t)
  (check-equal?(compare-lists? (make-slip FELLEISEN-COLOR "Wang" "Xi")list-test11) #f))

;__________________________________________@__________________________________________
#|
compare-deeper-list2?: slip los -> Boolean
GIVEN: list of slips
RETURNS: true iff slip found in list
EXAMPLES:
(get-felleisen-from1 yellow-list-test1)=> (list
 (make-slip FELLEISEN-COLOR "Wang" "Xi")
 (make-slip FELLEISEN-COLOR "Jones" "Tom")
 (make-slip FELLEISEN-COLOR "Tom" "Jone")
 (make-slip FELLEISEN-COLOR "Xi" "Wang")
 (make-slip FELLEISEN-COLOR "Shriram" "K."))))
STRATEGY: use of HOF ormap on lst2
|#
(define (compare-deeper-list2? s lst2)
  ;;GIVEN: slip
  ;;RETURNS:true iff slip is equal to s
  (ormap (lambda (s1)(or(equal? s s1) (check-mismatch-equal? s s1))) lst2))
;-----Tests:
(begin-for-test
  (check-equal?(compare-deeper-list2? (make-slip FELLEISEN-COLOR "Xi" "Wang") list-test11) #t)
  (check-equal?(compare-deeper-list2? (make-slip FELLEISEN-COLOR "Jones" "Tom") list-test11) #t))
