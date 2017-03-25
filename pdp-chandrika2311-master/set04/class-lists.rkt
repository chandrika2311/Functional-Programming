;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide felleisen-roster)
         ;shivers-roster)
         ;possible-roster?
         ;acceptable-felleisen-answer?
;;;;DATA DEFINITIONS:
#|
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
-- "yellow"
-- "blue"
TEMPLATE:

 color-fn : Color -> ??
;(define (color-fn c)
;  (cond
;    [(string=? c "yellow")    
;     ...]
;    [(string=? c "blue")
;     ...]))    

|#



(define-struct slip (color name1 name2))
;; A Slip is a (make-slip String String color)
;; Interpretation: 
;; name1: First name String
;; name2: Last name Srting
;;Color: accepts only "Yellow" or "Blue"
;; TEMPLATE
;; slip-fn : Slip -> ??
;;(define (slip-fn s)
;;  (... (slip-color s) (slip-name1 s)(slip-name2 s))


(define list-test1 (list
                    (make-slip "yellow" "Wang" "Xi")
                    (make-slip "yellow" "Jones" "Tom")
                    (make-slip "yellow" "Tom" "Jone")
                    (make-slip "yellow" "Xi" "Wang")
                    (make-slip "yellow" "Shriram" "K.")))
(define list-test4 (list
                    (make-slip "yellow" "Wang" "Xi")
                    (make-slip "blue" "Jones" "Tom")
                    (make-slip "blue" "Tom" "Jone")
                    (make-slip "yellow" "Xi" "Wang")
                    (make-slip "yellow" "Shriram" "K.")))

(define list-test2 (list
                    (make-slip "yellow" "Wang" "Xi")
                    (make-slip "yellow" "Shriram" "K.")))
(define list-test3 (list
                    (make-slip "yellow" "Jones" "Tom")
                    (make-slip "yellow" "Tom" "Jone")))
(define list-test5 (list
                    (make-slip "yellow" "Wang" "Xi")
                    (make-slip "yellow" "Jones" "Tom")
                    (make-slip "yellow" "Tom" "Jone")
                    (make-slip "yellow" "Xi" "Wang")
                    (make-slip "yellow" "Shriram" "K.")))

(define list-test6 (list
                    (make-slip "yellow" "Jones" "Tom")
                    (make-slip "yellow" "Tom" "Jones")
                    (make-slip "yellow" "Tom" "Jone")
                    (make-slip "blue" "Xi" "Wang")))
(define list-test7 (list
                    (make-slip "yellow" "Jones" "Tom")
                    (make-slip "yellow" "Tom" "Jone")
                    (make-slip "yellow" "chand" "shar")
                    (make-slip "yellow" "shar" "chand")
                    ))
(define list-test10 (list
                    (make-slip "yellow" "Jones" "Tom")
                    (make-slip "yellow" "Tom" "Jone")
                    (make-slip "yellow" "Shriram" "K.")))
(define list-test11 (list
                    (make-slip "yellow" "Wang" "Xi")
                    (make-slip "yellow" "Jones" "Tom")
                    (make-slip "yellow" "Tom" "Jone")
                    (make-slip "yellow" "Xi" "Wang")
                    (make-slip "yellow" "Shriram" "K.")))
(define list-test12 (list
                    (make-slip "blue" "Wang" "Xi")
                    (make-slip "blue" "Shriram" "K.")))


#|
no-duplicate? : s LOS -> Boolean
GIVEN: a list of slips and a slip
RETURNS: returns true if no duplicate in the entire list los for s
EXAMPLES:(no-duplicate? s los) => #f
STRATEGY: Use template for LOS on lst
|#

(define (no-duplicate? s los);; returns true if no duplicate in the entire list los for s
  (cond
    [(empty? los) #t]
    [(not (check-mismatch-equal? s (first los))) (no-duplicate? s (rest los))]
    [else #f]))

(begin-for-test
  (check-equal? (no-duplicate? (first list-test1) list-test1) #f))
#|
check-mismatch-equal? : s LOS -> Boolean
GIVEN: 2 slips 
RETURNS: returns true if they are equal
EXAMPLES:(check-mismatch-equal? (make-slip "yellow" "Jones" "Tom") (make-slip "yellow" "Shriram" "K.")) => #f
STRATEGY: Use template for LOS on lst
|#                                                             
(define (check-mismatch-equal? s1 s2)
  (and(string=? (slip-name1 s1) (slip-name2 s2)) (string=? (slip-name2 s1) (slip-name1 s2)))
   )
(begin-for-test
  (check-equal? (check-mismatch-equal? (make-slip "yellow" "Jones" "Tom") (make-slip "yellow" "Shriram" "K.")) #f) )

#|
felleisen-roster : ListOfSlip -> ListOfSlip
GIVEN: a list of slips
RETURNS: a list of slips containing all the students in Professor
Felleisen's class, without duplication.
EXAMPLES:(felleisen-roster list-test1)=> list-test2
STRATEGY: Use template for LOS on lst
|#
(define (felleisen-roster los)
  (cond
    [(empty? los) empty]
    [else (cond[(string=? "yellow" (slip-color (first los)))
                  (cond[(no-duplicate? (first los) (rest los))
                        (cons (first los) (felleisen-roster (rest los)))]
                   [else (felleisen-roster (rest los))]) 
                  ]
               [else  (felleisen-roster (rest los))])]))
(begin-for-test
  (check-equal? (felleisen-roster list-test1)
                (list (make-slip "yellow" "Jones" "Tom") (make-slip "yellow" "Tom" "Jone") (make-slip "yellow" "Xi" "Wang") (make-slip "yellow" "Shriram" "K.")))
  (check-equal? (felleisen-roster list-test4)
                (list  (make-slip "yellow" "Xi" "Wang") (make-slip "yellow" "Shriram" "K."))))

#|
shivers-roster: ListOfSlip -> ListOfSlip
GIVEN: a list of slips
RETURNS: a list of slips containing all the students in Professor
         Shivers' class, without duplication.
EXAMPLES:(shivers-roster list-test4)=>(list (make-slip "blue" "Jones" "Tom") (make-slip "blue" "Tom" "Jone")) ))
STRATEGY: Use template for LOS on lst


|#
(define (shivers-roster los)
  (cond
    [(empty? los) empty]
    [else (cond[(string=? "blue" (slip-color (first los)))
                  (cond[(no-duplicate? (first los) (rest los))
                        (cons (first los) (shivers-roster (rest los)))])

                       ;[else (shivers-roster (rest los))] 
                  ]
               [else  (shivers-roster (rest los))])]))

(begin-for-test(check-equal? (shivers-roster list-test4)(list (make-slip "blue" "Jones" "Tom") (make-slip "blue" "Tom" "Jone")) )
               (check-equal? (shivers-roster list-test3) empty )
               (check-equal? (shivers-roster list-test3) empty ))

;;check-slip-color: s los => Boolean
;;GIVEN : a slip and a list of slips
;;RETURNS: true iff colors in the list are same
;;STRATEGY: Use template for LOS on lst

(define (check-slip-color s los)
  (cond
    [(empty? los) #t]
    [(string=? (slip-color s)(slip-color(first los))) (check-slip-color s (rest los))]
    [else #f]))

(begin-for-test
(check-equal? (check-slip-color (first list-test3) (rest list-test3))
                #t)
(check-equal? (check-slip-color (first list-test4) (rest list-test4))
                #f))




#|
possible-roster? : ListOfSlip -> Boolean
GIVEN: a list of slips
RETURNS: true iff all the slips in the list are the same color,
                  and no student is represented twice.
EXAMPLES: (possible-roster? list-test3)=> #t
STRATEGY: Use template for LOS on lst
|#

(define (possible-roster? los)
  (cond
    [(empty? los) #t]
    [else (and(check-slip-color (first los) (rest los)) (no-duplicate? (first los) (rest los))) ]))

(begin-for-test
(check-equal? (possible-roster? list-test1 ) #f)
(check-equal? (possible-roster? list-test2 ) #t))
#|
acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
GIVEN: two lists of slips, lst1 and lst2
RETURNS: true iff every student on a yellow slip in lst1 appears once
and only once in lst2.
EXAMPLES:(acceptable-felleisen-answer? list-test1  empty ) => #f
STRATEGY: Use template for LOS on lst
|#

(define (acceptable-felleisen-answer? lst1 lst2 )
  (calling-comparision-list? (felleisen-roster(get-felleisen lst1))  lst2))

(begin-for-test(check-equal? (acceptable-felleisen-answer? list-test1  empty ) #f)
               (check-equal? (acceptable-felleisen-answer? list-test10 list-test11) #t)
               (check-equal? (acceptable-felleisen-answer? list-test1 list-test3) #f)
               (check-equal?(acceptable-felleisen-answer? list-test6 list-test7) #t))

#|
get-felleisen: los -> los
GIVEN: LIST OF SLIPS
RETURNS: List Of Slips
EXAMPLES:
(get-felleisen-from1 list-test1)=> (list
 (make-slip "yellow" "Wang" "Xi")
 (make-slip "yellow" "Jones" "Tom")
 (make-slip "yellow" "Tom" "Jone")
 (make-slip "yellow" "Xi" "Wang")
 (make-slip "yellow" "Shriram" "K."))))
STRATEGY: Use template for LOS on lst

|#
(define (get-felleisen los)
  (cond
    [(empty? los) empty]
    [(string=?  (slip-color(first los)) "yellow") (cons (first los)(get-felleisen (rest los)) ) ]
    [else (get-felleisen (rest los))])
  )
(begin-for-test
(check-equal? (get-felleisen list-test1) (list
 (make-slip "yellow" "Wang" "Xi")
 (make-slip "yellow" "Jones" "Tom")
 (make-slip "yellow" "Tom" "Jone")
 (make-slip "yellow" "Xi" "Wang")
 (make-slip "yellow" "Shriram" "K."))))

#|
calling-comparision-list?:los los -> Boolean
GIVEN: 2 LISTs OF SLIPS
RETURNS:Boolean
EXAMPLES:
(calling-comparision-list? list-test10 list-test11)=>#t
STRATEGY: Use template for LOS on lst

|#
(define (calling-comparision-list? lst1 lst2)
   (cond[(empty? lst1) #t]
        [(compare-lists? (first lst1) lst2)(calling-comparision-list?(rest lst1) lst2)]
        [else #f]

  ))

(begin-for-test
 (check-equal? (calling-comparision-list? list-test10 list-test11) #t)
 (check-equal? (calling-comparision-list? list-test1 list-test3) #f))
 
#|
compare-lists?:slip los -> Boolean
GIVEN: LIST OF SLIPS
RETURNS: Boolean
EXAMPLES:
(compare-lists? (first list-test10)  list-test11)=> #t
(compare-lists? (make-slip "yellow" "chan" "sha")  list-test11)=> #f
STRATEGY: Use template for LOS on lst

|#
 (define (compare-lists? s lst2)
    (cond[(empty? lst2) #f]
         [(not(equal? s (first lst2))) (cond[(and(string=? (slip-color s)(slip-color (first lst2)))
                                                  (string=? (slip-name1 s) (slip-name2 (first lst2)))
                                                  (string=? (slip-name2 s) (slip-name1 (first lst2))))
                                              (not (compare-deeper-list2? s (rest lst2)))]
                                             [else (compare-lists? s (rest lst2))])]
                                             
         [(and(equal? s (first lst2)) (not(compare-deeper-list2? s (rest lst2)))) #t]
         ;[else (compare-lists? s (rest lst2))]
         ))
 
(begin-for-test
  (check-equal?(compare-lists? (first list-test10)  list-test11) #t)
  (check-equal?(compare-lists? (make-slip "yellow" "chan" "sha")  list-test11) #f))

;(not(and(string=? (slip-name1 s) (slip-name2 (first los)))(string=? (slip-name2 s) (slip-name1 (first los)))))

#|
compare-deeper-list2?: slip los -> Boolean
GIVEN: list of slips
RETURNS: Boolean
EXAMPLES:
(get-felleisen-from1 list-test1)=> (list
 (make-slip "yellow" "Wang" "Xi")
 (make-slip "yellow" "Jones" "Tom")
 (make-slip "yellow" "Tom" "Jone")
 (make-slip "yellow" "Xi" "Wang")
 (make-slip "yellow" "Shriram" "K."))))
STRATEGY: Use template for LOS on lst

|#
 (define (compare-deeper-list2? s lst2)
(cond[(empty? lst2) #f]
         [(not(equal? s (first lst2))) (cond[(and(string=? (slip-color s)(slip-color (first lst2)))
                                                  (string=? (slip-name1 s) (slip-name2 (first lst2)))
                                                  (string=? (slip-name2 s) (slip-name1 (first lst2))))
                                              #t]
                                             [else (compare-deeper-list2? s (rest lst2))])]
                                             
         [(equal? s (first lst2)) #t ]))

(begin-for-test
  (check-equal?(compare-deeper-list2? (make-slip "yellow" "Xi" "Wang") list-test11) #t)
  (check-equal?(compare-deeper-list2? (make-slip "yellow" "Jones" "Tom") list-test11) #t))


;(compare-deeper-list2? s (rest lst2))