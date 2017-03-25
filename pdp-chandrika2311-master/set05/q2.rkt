;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide make-enrollment
         enrollment-student
         enrollment-class
         make-roster
         roster-classname
         roster-students
         behavior-correct?
         enrollments-to-rosters
         enrollments-to-rosters-bad-1
         enrollments-to-rosters-bad-2
         enrollments-to-rosters-bad-3)

;###################################################################################################
;;                                         DEFINITIONS:
;###################################################################################################
;;A SetOfStudents(SOS) is a Unique ListOfStudents
;;Empty is a SetOfStudents

;; ListofStudents

;; A SetOfStudents (SOS) is either
;; -- empty
;; -- (cons Students SOS)

;; sos-fn : SOS -> ??
;; (define (sos-fn los)
;;   (cond
;;     [(empty? sos) ...]
;;     [else (...
;;             (first sos))
;;             (sos-fn (rest sos)))]))
(define-struct enrollment (student class))
;; An Enrolment is a 
;;  (make-enrolment String String)
;; Interpretation:
;; Student is the student's name
;; Class is the Class name that the student is enrolled into

;; enrolment-fn : Enrolment -> ??
;; (define (enrolment-fn e)
;;   (... (enrolment-student e) (enrolment-class e)))
;__________________________________________@__________________________________________
;; ListofEnrollmentAssertion(LOE)
;__________________________________________@__________________________________________
;; A SetOfEnrollmentAssertion (LOE) is either
;; -- empty
;; -- (cons Enrolment LOE)

;; soe-fn : SOE -> ??
;; (define (soe-fn loe)
;;   (cond
;;     [(empty? loe) ...]
;;     [else (...
;;             (first sos))
;;             (los-fn (rest sos)))]))
;__________________________________________@__________________________________________
(define-struct roster (classname students))
;; An Roster is a 
;;  (make-roster String SetOfStudents)
;; Interpretation:
;; Classname: is the Class name that the student is enrolled into
;; Students:  SetOfStudents(SOS) is a unique list of student names
;; enrolment-fn : Enrolment -> ??
;; (define (enrolment-fn e)
;;   (... (enrolment-student e) (enrolment-class e)))
;__________________________________________@__________________________________________
;; SetOfClassRosterAssertion(LOR)
;__________________________________________@__________________________________________
;; A SetOfClassRosterAssertion (LOR) is either
;; -- empty
;; -- (cons Roster LOR)

;; lor-fn : LOR -> ??
;; (define (lor-fn lor)
;;   (cond
;;     [(empty? lor) ...]
;;     [else (...
;;             (first lor))
;;             (lor-fn (rest lor)))]))
;###################################################################################################
;...........................................Test Cases.............................................
;###################################################################################################
(define soe1(list (make-enrollment "John" "PDP")
                  (make-enrollment "Kathryn" "Networks")
                  (make-enrollment "Feng" "PDP")
                  (make-enrollment "Amy" "PDP")
                  (make-enrollment "Amy" "Networks")))
(define soe3(list (make-enrollment "John" "PDP")
                  (make-enrollment "Kathryn" "Networks")
                  (make-enrollment "Feng" "PDP")
                  (make-enrollment "Amy" "PDP")
                  (make-enrollment "Amy" "Networks")))
(define soe4(list (make-enrollment "John" "PDP")
                  (make-enrollment "Feng" "PDP")
                  (make-enrollment "Amy" "PDP")))
(define soe6(list (make-enrollment "John" "PDP")
                  (make-enrollment "Kathryn" "Networks")
                  (make-enrollment "Kathryn" "PDP")
                  (make-enrollment "Feng" "PDP")
                  (make-enrollment "Amy" "PDP")
                  (make-enrollment "Amy" "Networks")))
(define sos-1 (list "John" "Feng" "Amy"))
(define sos-2 (list "Kathryn" "Amy"))
(define sor-1 (list
               (make-roster "PDP" (list "John" "Feng" "Amy"))
               (make-roster "Networks" (list "Kathryn" "Amy"))))
(define sor-bad1 (list
                  (make-roster "PDP"(list "Kathryn" "Amy"))
                  (make-roster "Networks" (list "Kathryn" "Amy"))))
(define sor-2 (list
               (make-roster "Networks" (list "Kathryn" "Amy"))))
(define sor-3 (list
               (make-roster "PDP" (list "John" "Kathryn" "Feng" "Amy"))
               (make-roster "Networks" (list "Kathryn" "Amy"))))
;__________________________________________@______________________________________
#|
enrollments-to-rosters: SetOfEnrollmentAssertion-> SetOfClassRosterAssertion
GIVEN: SetOfEnrollmentAssertion
RETURN: SetOfClassRosterAssertion
Examples:(enrollments-to-rosters soe3) => sor-1
STRATEGY: Use of HOF Foldr on soe
|#
(define (enrollments-to-rosters soe)
  ;;GIVEN:enrollment setOfRosters
  ;;RETURNS: SetOfRosters with one element with one classname
  (foldr (lambda (e sor) (create-roster-one-element e soe sor)) empty soe))
;-----Tests:
(begin-for-test(check-equal?(enrollments-to-rosters soe3) sor-1))
(begin-for-test(check-equal?(enrollments-to-rosters soe6) sor-3))

;__________________________________________@______________________________________
#|
create-roster-one-element: e soe sor-> sor
GIVEN: An EnrollmentAssertion, SetOfEnrollmentAssertion,SetOfClassRosterAssertion
RETURN: returns SetOfClassRosterAssertion with classname and set of students of
        EnrollmentAssertion from SetOfEnrollmentAssertion
Examples:(create-roster-one-element (make-enrollment "John" "PDP") soe3 sor-2) sor-1)
STRATEGY: Cases on e
|#
(define (create-roster-one-element e soe sor)
  (if(check-duplicate? e sor)
     sor
     (cons(make-roster (enrollment-class e ) (create-sos e soe)) sor)
     ))
;----Tests
(begin-for-test
  (check-equal? (create-roster-one-element (make-enrollment "John" "PDP") soe3 sor-2) sor-1))
;__________________________________________@______________________________________
#|
check-duplicate?: e sor-> Boolean
GIVEN: An EnrollmentAssertion, SetOfClassRosterAssertion
RETURN: returns true iff classname of e already present in sor
Examples:(check-duplicate?(make-enrollment "John" "PDP")sor-3)=> #t
STRATEGY: Use of HOF ormap on sor
|#
(define (check-duplicate? e sor)
  ;;Given: Enrollment
  ;;RETURNS: True iff class of enrollment is same as classname of roster
  (ormap(lambda (e1)(string=? (enrollment-class e)(roster-classname e1))) sor))
;----Tests
(begin-for-test(check-equal?(check-duplicate?(make-enrollment "John" "PDP")sor-3) #t)
               (check-equal?(check-duplicate?(make-enrollment "John" "Networks")sor-3) #t))
;__________________________________________@______________________________________  
#|
create-sos: e soe-> sos
GIVEN: An EnrollmentAssertion, SetOfEnrollmentAssertion
RETURN: returns true iff classname of e already present in sor
Examples:(create-sos (make-enrollment "John" "PDP") soe3)=> sos-1
STRATEGY: Use of HOF foldr on soe
|#
(define (create-sos e soe)
  ;;GIVEN: enrollment and a SetOfEnrollments
  ;;RETURNS: SetOfStudents with class in SetOfEnrollment
  (foldr(lambda(e1 sos) (if(string=?(enrollment-class e )(enrollment-class e1) )
                           (cons(enrollment-student e1) sos) sos)) empty soe))

;----Tests
(begin-for-test(check-equal? (create-sos (make-enrollment "John" "PDP") soe3)sos-1))
;__________________________________________@______________________________________
;;enrollments-to-rosters-bad-1: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;;GIVEN: An EnrollmentAssertion, SetOfEnrollmentAssertion
;;RETURN: returns soe classname in enrollmentAssertions
;;Examples:(enrollments-to-rosters-bad-1 soe3) => empty
;;STRATEGY: combine simpler functions
(define (enrollments-to-rosters-bad-1 soe)
  empty)
(begin-for-test(check-equal? (enrollments-to-rosters-bad-1 soe3) empty))
;__________________________________________@______________________________________
;;enrollments-to-rosters-bad-2: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;;GIVEN: An EnrollmentAssertion, SetOfEnrollmentAssertion
;;RETURN: returns soe classname in enrollmentAssertions

(define (enrollments-to-rosters-bad-2 soe)
  ;;GIVEN:  enrollment setOfRosters
  ;;RETURNS:Adding an entry to SetOfRosters for enrollment-class
  (foldr (lambda (e sor) (create-roster-one-element-2 e soe sor)) empty soe))
(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-2 soe3)
                (list (make-roster "PDP" (list "Kathryn" "Amy"))
                      (make-roster "Networks" (list "John" "Feng" "Amy")))))

;__________________________________________@______________________________________
;;create-sos-2: Enrollment SetOfEnrollmentAssertion -> SetOfStudents
;;GIVEN: An EnrollmentAssertion, SetOfEnrollmentAssertion
;;RETURN: returns SetOfStudents with the different class as in Enrollment-class
;;EXAMPLES:(create-sos-2 (make-enrollment "John" "PDP") soe3)=>(list "Kathryn" "Amy")
;;STRATEGY:Use of HOF Foldr on soe

(define (create-sos-2 e soe)
  ;;GIVEN: enrollment
  ;;RETURNS: Set OF Students where class not equal to enrollment-class
  (foldr(lambda(e1 sos) (if(not(string=?(enrollment-class e )(enrollment-class e1)))
                           (cons(enrollment-student e1) sos) sos)) empty soe))
(begin-for-test(check-equal?(create-sos-2 (make-enrollment "John" "PDP") soe3)(list "Kathryn" "Amy")))
;__________________________________________@______________________________________
;;create-roster-one-element-2: Enrollment SetOfClassRosterAssertion SetOfEnrollmentAssertion
;;-> SetOfStudents
;;GIVEN: An EnrollmentAssertion, SetOfEnrollmentAssertion SetOfClassRosterAssertion
;;RETURN: returns SetOfStudents with the different class as in Enrollment-class iff there isnt
;already an elemnet with same classname in the roster set
;;EXAMPLES:(create-sos-2 (make-enrollment "John" "PDP") soe3)=>(list "Kathryn" "Amy")
;;STRATEGY:Combine Simpler FUnctions

(define (create-roster-one-element-2 e soe sor)
  (if(check-duplicate? e sor)
     sor
     (cons(make-roster (enrollment-class e ) (create-sos-2 e soe)) sor)
     ))
;__________________________________________@______________________________________
;;enrollments-to-rosters-bad-3: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;;GIVEN: An EnrollmentAssertion, SetOfEnrollmentAssertion
;;RETURN: returns soe classname in enrollmentAssertions
;;Examples:(enrollments-to-rosters soe3) => sor-1
;;STRATEGY: Use of HOF Foldr on soe
(define (enrollments-to-rosters-bad-3 soe)
  ;;GIVEN: enrollment
  ;;RETURNS: SetOfRosters where classname = enrollment-class and sos = empty
  (foldr (lambda (e sor) (create-roster-one-element-3 e soe sor)) empty soe))
(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-3 soe3)
                (list (make-roster "PDP" '()) (make-roster "Networks" '()))))
;__________________________________________@______________________________________
;create-roster-one-element-3: e soe sor-> sor
;GIVEN: An EnrollmentAssertion, SetOfEnrollmentAssertion,SetOfClassRosterAssertion
;RETURN: returns SetOfClassRosterAssertion with classname and empty set of students
;Examples:(create-roster-one-element (make-enrollment "John" "PDP") soe3 sor-2) sor-1)
;STRATEGY: Cases on e
(define (create-roster-one-element-3 e soe sor)
  (if(check-duplicate? e sor)
     sor
     (cons(make-roster (enrollment-class e ) empty ) sor)
     ))
(begin-for-test
  (check-equal? (create-roster-one-element-3(make-enrollment "John" "PDP") soe3 sor-3 )
                sor-3)
  (check-equal? (create-roster-one-element-3(make-enrollment "John" "PDP1") soe3 sor-3 )
                (list (make-roster "PDP1" '())
                      (make-roster "PDP" (list "John" "Kathryn" "Feng" "Amy"))
                      (make-roster "Networks"(list "Kathryn" "Amy")))))
;__________________________________________@______________________________________
#|
equal-roster?: roster1 roster2-> Boolean
;GIVEN:  given 2 rosters to check for eqality
;RETURNS:true iff first roster classname equal to second roster classname 
;;EXAMPLE:(equal-roster? (make-roster "PDP" (list "John" "Feng" "Amy"))
                                            (make-roster "PDP" (list "John" "Feng" "Amy")))=> #t
;;STRATEGY: combine simpler functions
|#
(define (equal-roster? r1 r2)
  (and(roster-classnames? (roster-classname r1) (roster-classname r2))
      (check-students? (roster-students r1)(roster-students r2))))

(define sor-11 (list
                (make-roster "PDP" (list "John" "Feng" "Amy"))
                (make-roster "Networks" (list "Kathryn" "Amy"))))
(define sor-21 (list
                (make-roster "PDP" (list "John" "Feng" "Amy"))
                (make-roster "Networks" (list "Kathryn" "Amy"))))
;----Tests
(begin-for-test(check-equal? (equal-roster? (make-roster "PDP" (list "John" "Feng" "Amy"))
                                            (make-roster "PDP" (list "John" "Feng" "Amy"))) #t))
(begin-for-test(check-equal? (equal-roster? (make-roster "PDP" (list "John" "Feng" "Amy"))
                                            (make-roster "Networks" (list "John" "Feng" "Amy"))) #f))
;__________________________________________@______________________________________
;;check-students? SetOfStudents1 SetOfStudents2-> Boolean
;;GIVEN:2 sets of students to check equality from
;;RETURNS: true iff all elements of first list in the second list
;;EXAMPLES:
;;(check-students?(list "John" "Amy")(list "John" "chandrika" "Amy"))#t
;;(check-students?(list "John" "chandrika" "Amy")(list "John" "Amy"))#f
;;STRATEGY: Use of HOF andmap on sos(SetOfStudents)
(define(check-students? l1 l2)
;;GIVEN:  list1
;;RETURNS:true iff student in list1 
  (andmap(lambda (s1) (check-every-student? s1 l2)) l1))
;----Tests
(begin-for-test(check-equal?(check-students?(list "John" "Amy")(list "John" "chandrika" "Amy"))#t))
(begin-for-test(check-equal?(check-students?(list "John" "chandrika" "Amy")(list "John" "Amy"))#f))
;__________________________________________@______________________________________
;;check-every-student? Name SetOfStudents2-> Boolean
;;GIVEN:a student and set of students to check equality from
;;RETURNS: true iff student present in the second list
;;EXAMPLES:
;;(check-every-student? "John"(list "John" "Amy"))#t
;;(check-every-student? "Chan"(list "John" "Amy"))#f
;;STRATEGY: Use of HOF ormap on sor(SetOfRosters)
(define (check-every-student? s l2)
  ;;GIVEN: Student
  ;;RETURNS: true iff student is equal to s1
  (ormap(lambda (s1)(string=? s s1)) l2))
;----Tests
(begin-for-test(check-equal?(check-every-student? "John"(list "John" "Amy")) #t)
               (check-equal?(check-every-student? "Chan"(list "John" "Amy")) #f))

;__________________________________________@______________________________________
;;roster-classnames? classname1 classname2-> Boolean
;;GIVEN:2 classnames to compare
;;RETURNS: true iff both classnames equal
;;EXAMPLES:
;;(roster-classnames?"Networks" "Networks")=>#t
;STRATEGY: Combine Simpler Functions
(define (roster-classnames? c1 c2)
  (string=? c1 c2))
;----Tests
(begin-for-test
  (check-equal?(roster-classnames?"Networks"
                                  "Networks")#t))
;__________________________________________@______________________________________
;;my-member? roster sor-> Boolean
;;GIVEN:1 roster and a SetOfRoster to compare
;;RETURNS: true if roster found in sor
;;EXAMPLES:
;;(my-member? (make-roster "Networks" (list "Kathryn" "Amy")) sor-11)=>#t
;;STRATEGY: Use of HOF ormap on sor(SetOfRosters)

(define (my-member? r sor1)
;;GIVEN: roster
  ;;RETURNS: false if roster not equal to r1
  (ormap(lambda (r1) (equal-roster? r r1))sor1))
;----Tests
(begin-for-test(check-equal?(my-member? (make-roster "Networks" (list "Kathryn" "Amy")) sor-11)#t))
;__________________________________________@______________________________________
;;subset? sor1 sor2-> Boolean
;;GIVEN:2 SetOfRoster to compare
;;RETURNS: true if all elements of sor1 in sor2
;;EXAMPLES:
;;(my-member? (make-roster "Networks" (list "Kathryn" "Amy")) sor-11)=>#t
;;STRATEGY: Use of HOF andmap on sor(SetOfRosters)
(define (subset? sor1 sor2)
  ;;GIVEN: roster
  ;;RETURNS: True iff roster is a member of SetOfRosters
  (andmap(lambda (r) (my-member? r sor2))sor1))
;----Tests
(begin-for-test(check-equal?(subset? sor-11 sor-11) #t))
(begin-for-test(check-equal?(subset? sor-1 sor-3) #t))
(begin-for-test(check-equal?(subset? sor-3 sor-1) #f))
;__________________________________________@______________________________________
; set-equal? : SetOfRoster SetOfRoster -> Boolean
;;GIVEN:2 SetOfRoster to compare
;;RETURNS: true if all elements of sor1 in sor2 and vice-versa
;;EXAMPLES:
;(set-equal? sor-11 sor-11) => #t
;(set-equal? sor-1 sor-3) =#f
;; STRATEGY: Combine simpler functions
(define (set-equal? sor1 sor2)
  (and
   (subset? sor1 sor2)
   (subset? sor2 sor1)))
;----Tests
(begin-for-test(check-equal?(set-equal? sor-11 sor-11) #t))
(begin-for-test(check-equal?(set-equal? sor-1 sor-3) #f))
;__________________________________________@______________________________________
#|
behavior-correct? : ProposedSolution SetOfEnrollmentAssertion -> Boolean
GIVEN: a ProposedSolution soln-fn and a SetOfEnrollmentAssertion se
RETURNS: true iff the output of soln-fn on se is an example of correct
behavior by a ProposedSolution.
EXAMPLE: See example above
|#
(define(behavior-correct? soln-fn soe)
  (set-equal? (soln-fn soe) (enrollments-to-rosters soe)))
;----Tests
(begin-for-test (check-equal? (behavior-correct? enrollments-to-rosters soe1)#t))
(begin-for-test (check-equal? (behavior-correct? enrollments-to-rosters-bad-2 soe1)#f))
(begin-for-test (check-equal? (behavior-correct? enrollments-to-rosters-bad-3 soe1)#f))
(begin-for-test (check-equal? (behavior-correct? enrollments-to-rosters soe3)#t))