;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide program-to-strings make-def make-varexp make-appexp)
(define INDENT 4)

;; A Program is a ListOfDefinition.

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

(define-struct varexp (name))
(define-struct appexp (fn args)) 


(define sample-program
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z))))))))
;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; A Variable is a Symbol.

;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: GarterSnake program and a width
;; RETURNS: A representation of the program as a sequence of lines following the
;;          assigned formatting rules
;; DESIGN STRATEGY: Combine simpler functions
(define (program-to-strings p width)
  (cond
    [(empty? p) empty]
    [else (generate-lines p width)]
    )
  )
#|
(begin-for-test (check-equal?
                 (program-to-strings (list
                                      (make-def 'a-very-long-function-name
                                                (list 'x 'y) empty)
                                      (make-def 'a-very-long-function-name1
                                                (list 'x1 'y1) empty)
                                      (make-def 'a-very-long-function-name2
                                                (list 'x2 'y2) empty)
                                      (make-def 'a-v(list 'x 'y) empty)) 20)
                 (list"def a-very-long-function-name (x,"
                      "                               y) :"
                      "def a-very-long-function-name1 (x1,"
                      "                                y1) :"
                      "def a-very-long-function-name2 (x2,"
                      "                                y2) :"
                      "def a-v (x,y) :"))
                (check-equal?(program-to-strings empty 20) empty))|#
;__________________________________________________________________________________
;; generate-lines : Program PosInt -> ListOfString
;; GIVEN: A Gartersnake program and a width
;; RETURNS: A ListOfString representing a properly formatted version of the program
;; HALTING CONDITION: p is empty
;; DESIGN STRATEGY: Combine simpler functions
(define (generate-lines p width)
  (cond
    [(empty? p) empty]
    [else (append(generate-def (first p) width) (generate-lines (rest p) width))]
    ;generate-def handles creating the string for each individual definition, cons these together
    )
  )

(begin-for-test (check-equal?
                 (generate-lines (list
                                      (make-def 'a-very-long-function-name
                                                (list 'x 'y) (make-appexp 'f1 (list(make-varexp 'y))))
                                      (make-def 'a-very-long-function-name1
                                                (list 'x1 'y1)(make-appexp 'f1 (list (make-varexp 'y)))))20)
                 (list"def a-very-long-function-name (x,"
                      "                               y) :"
                      "    f1(y)"
                      "def a-very-long-function-name1 (x1,"
                      "                                y1) :"
                      "    f1(y)")))

;__________________________________________________________________________________
;; generate-def : Definition PosInt -> String
;; GIVEN: A definition and a posint representing width
;; RETURNS: A string which represents a properly formatted version of the given definition
;; DESIGN STRATEGY: Combine simpler functions
(define (generate-def d width)
  (append(generate-func d width)(cons (generate-body (def-body d) width) '()))
  )
 
(begin-for-test(check-equal?(generate-def(make-def 'a-very-long-function-name
                                                (list 'x 'y) (make-appexp 'f1 (list(make-varexp 'y))))20)
                          (list "def a-very-long-function-name (x,"
                                  "                               y) :"
                                  "    f1(y)"))
               
               (check-equal?(generate-def(make-def 'a-very-long-function-name
                                                   (list 'x 'y)
                                                   (make-appexp 'f1 (list (make-varexp 'y))))20)
                            (list "def a-very-long-function-name (x,"
                                  "                               y) :"
                                  "    f1(y)")))

;__________________________________________________________________________________
;; generate-func : Definition PosInt -> String or List of String
;; GIVEN: A definition and a PosInt representing width
;; RETURNS: Case: when the string length of definition is within with width
;;                , A String representing the function name and arguments part of
;;                  the given definition formatted properly in one line
;;          Case2: when the string length of definition is not within with width
;;                , A String representing the function name and arguments part of
;;                  the given definition formatted properly in multiple lines hence returning a
;;                  list of strings
;; DESIGN STRATEGY: Divide into cases on (string-length (first (func-possibility-1 def)))
(define (generate-func def width)
  (cond
    [(<= (string-length (first (func-possibility-1 def))) width) (func-possibility-1 def)]
    ;[(<= (string-length (first (func-possibility-1 def))) width) (func-possibility-1 def)]
    ;;if function name & args can fit on one line without violating width, use this clause
    [else (func-possibility-2 def width)]  ;;else clause for case where function name & args cannot 
    ;fit within given width    
    )
  )
;________Tests
(begin-for-test(check-equal?(generate-func(make-def 'a-v(list 'x 'y) empty)20)
                           (list "def a-v (x,y) :"))
               (check-equal?(generate-func(make-def 'a-very-long-function-name(list 'x 'y) empty)20)
                            (list "def a-very-long-function-name (x,"
                                  "                               y) :")))

;__________________________________________________________________________________
;; func-possibility-1 : Definition -> String
;; GIVEN: A definition
;; RETURNS: A string representing the first possibility for a function name
;;          and arguments, where they can all fit onto a single line
;; DESIGN STRATEGY: Combine simpler functions
(define (func-possibility-1 d)
  (cons(string-append "def " (symbol->string (def-name d)) " (" (arg-list-1 (def-args d))) empty)
  )
;________Tests
(begin-for-test(check-equal?
                (func-possibility-1(make-def 'f1 (list 'a 'b 'c 'd)
                                             (make-appexp 'f2 (list (make-varexp 'z)))))
                (list "def f1 (a,b,c,d) :")))

;__________________________________________________________________________________
;; arg-list-1 : ListOfVariable -> String
;; GIVEN: A listofvariable
;; RETURNS: A string representing a list of arguments, to be used with
;;          the "func-possibility-1" function
;; HALTING CONDITION: given listofvariable is empty
;; DESIGN STRATEGY: Divide into cases on size of lov
(define (arg-list-1 lov)
  (cond
    [(empty? lov) ") :"]
    [(empty? (rest lov)) (string-append (symbol->string (first lov)) (arg-list-1 (rest lov)))]
    [else (string-append (symbol->string (first lov)) "," (arg-list-1 (rest lov)))]
    )
  )

(define sample-def-1 (make-def 'f1 (list 'a 'b 'c 'd) (make-appexp 'f2 (list (make-varexp 'z)))))
;________Tests
(begin-for-test (check-equal? (func-possibility-1 sample-def-1) (list "def f1 (a,b,c,d) :")))

;__________________________________________________________________________________

;; func-possibility-2 : Definition PosInt -> ListOfString
;; GIVEN: A definition and a posint representing width
;; RETURNS: A listofstring representation of the given definition's function name & arguments
;; DESIGN STRATEGY: Use template for def on d
(define (func-possibility-2 d width)
  (cons (string-append "def " (symbol->string (def-name d))
                       " (" (symbol->string (first (def-args d))) ",")
        (arg-list-2 (rest (def-args d))
                    (string-length (string-append "def "
                                                  (symbol->string (def-name d)) "(")))) 
  )
;________Tests
(define sample-def-11 (make-def 'hello (list 'a 'b 'c 'd)
                               (make-appexp 'f2 (list (make-varexp 'z)))))
(begin-for-test (check-equal?
                 (func-possibility-2 sample-def-11 10)
                 (list "def hello (a,"
                       "           b,"
                       "           c,"
                       "           d) :")))

;; arg-list-2 : ListOfVariable PosInt -> ListOfString
;; GIVEN: A listofvariable representing the args not yet added to the listofstring
;;        and a posint representing the amount that they should be indented
;; RETURNS: A listofstring representing the proper formatting for the args
;; HALTING CONDITION: final arg of lov is reached
;; DESIGN STRATEGY: Combine simpler functions
(define (arg-list-2 lov indent)
  (cond
    [(empty? lov) empty]
    [(empty? (rest lov))(cons (string-append (make-indent-spaces-func indent)
                                             (symbol->string (first lov))
                                             ") :")
                              (arg-list-2 (rest lov) indent))]
    [else (cons (string-append (make-indent-spaces-func indent) (symbol->string (first lov)) ",")
                (arg-list-2 (rest lov) indent))]  
    )
  )
;________Tests
(begin-for-test(check-equal?(arg-list-2(list 'a 'b 'c 'd) 5)
                            (list "      a,"
                                  "      b,"
                                  "      c,"
                                  "      d) :"))
               )


;__________________________________________________________________________________
;; make-indent-spaces-func : PosInt -> String
;; GIVEN: A PosInt representing the desired number of spaces for an indent 
;; WHERE: indent is at least 1
;; RETURNS: A string representing the desired indent
;; HALTING CONDITION: indent reaches 1
;; DESIGN STRATEGY: Divide into cases on the size of indent
(define (make-indent-spaces-func indent)
  (cond
    [(equal? indent 0) " "]
    [else (string-append " " (make-indent-spaces-func (- indent 1)))] 
    )
  )
;________Tests
(begin-for-test (check-equal? (func-possibility-2 sample-def-1 5)
                              (list "def f1 (a,"
                                    "        b,"
                                    "        c,"
                                    "        d) :")))

;__________________________________________________________________________________
;; generate-body : Exp PosInt -> ListOfString
;; GIVEN: A definition and a posint representing width
;; RETURNS: A listofstring representing the body of the given definition
;; DESIGN STRATEGY: Call a more general function
(define (generate-body b width)
  (print-body-top b width INDENT))

#|
;__________________________________________________________________________________
;;body-varexp-possibility: exp-> string
;; GIVEN: An expression which is the body of a definition
;; RETURNS: A string representing the body
(define (body-varexp-possibility e)
  (string-append (make-indent-spaces-func INDENT)
                 (symbol->string (varexp-name e))
                 ")"))
(begin-for-test(check-equal?(body-varexp-possibility (make-varexp 'z))"     z)"))
;__________________________________________________________________________________
;; body-possibility-1 : Exp -> String
;; GIVEN: An expression which is the body of a definition
;; RETURNS: A string representing the body 
(define (body-possibility-1 e)
  (string-append (make-indent-spaces-func INDENT)
                 (symbol->string (appexp-fn e))
                 "("
                 (expressions-printer (appexp-args e)))   
  )
;________Tests:
(begin-for-test(check-equal?(body-possibility-1(make-appexp 'f1
                                                            (list (make-varexp 'x)))) "     f1(x)"))
|#

;; fix-output -> ListOfString -> ListOfString
;; GIVEN: A listofstring
;; RETURNS: A listofstring
;; DESIGN STRATEGY: Divide into cases on l being a string, list, or empty list
;; EXAMPLE: (fix-output (list "a" "b" (list "c" "d") "e")) returns (list "a" "b" "c" "d" "e")
(define (fix-output l) ;;function added to rectify problem where the body being built would have lists of string inside the list of string
  (cond
    [(empty? l) empty]
    ;[(string? l) l]
    [(list? (first l)) (append (first l) (fix-output (rest l)))]  
    [else (cons (first l) (fix-output (rest l)))]   
    )
  )

(begin-for-test (check-equal? (fix-output (list "a" "b" (list "c" "d") "e")) (list "a" "b" "c" "d" "e")))

;; make-indent-spaces-func2 : PosInt -> String
;; GIVEN: A PosInt representing the desired number of spaces for an indent 
;; WHERE: indent is at least 1
;; RETURNS: A string representing the desired indent
;; HALTING CONDITION: indent reaches 1
;; DESIGN STRATEGY: Divide into cases on indent
;; EXAMPLE: (make-indent-spaces-func2 3) returns "   "
(define (make-indent-spaces-func2 indent)
  (cond
    [(equal? indent 1) " "]
    [else (string-append " " (make-indent-spaces-func2 (- indent 1)))]   
    )
  )

;; print-body-top : Exp PosInt PosInt -> ListOfString
;; GIVEN: An exp, a posint representing width and a posint representing
;;        current indent
;; RETURNS: A ListOfString representing the given exp as a function's body
;; DESIGN STRATEGY: Divide into cases on string length
;; EXAMPLE: (print-body-top test-appexp 500 4) returns "    f1(x, y, f2(z, y), z, a, b, c, d)"))
(define (print-body-top e width indent) ;;indent = 4 for initial call
  (cond
    [(<= (string-length (print-appexp-option-1-top e indent)) width) (print-appexp-option-1-top e indent)] ;;option where the entire appexp all fits on 1 line
    [(<= (string-length (string-append (make-indent-spaces-func2 indent) (symbol->string (appexp-fn e)) "(" (print-first-arg (first (appexp-args e))) ",")) width)
     (fix-output (print-appexp-option-2-top e indent width))] ;;option where the initial appexp qualifies for option 2 
    [else (fix-output (print-appexp-option-3-top e indent width))] ;;option where appexp must use option 3
    )
  )


;;-------------------print-body-top testing--------------------------------;
(begin-for-test (check-equal? (print-body-top test-appexp 500 4) "    f1(x, y, f2(z, y), z, a, b, c, d)"))
;(begin-for-test (check-equal? (print-body-top test-appexp-2 20 4) (list "    f1(f2(z, y)," "       z," "       f1(f2(z, y)," "          z))")))
(begin-for-test (check-equal? (print-body-top test-appexp 10 4)
                              (list "    f1(x," "       y,""       f2" "        (z," "         y)" "       z," "       a," "       b," "       c," "       d)")))
(begin-for-test (check-equal? (print-body-top test-appexp-q 5 4) (list "    f1" "     (z," "      a," "      b)")))     

(define test-appexp-q (make-appexp 'f1 (list (make-varexp 'z) (make-varexp 'a) (make-varexp 'b))))

(define test-appexp
  (make-appexp
              'f1
              (list (make-varexp 'x)
                    (make-varexp 'y)
                    (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-varexp 'z)
                    (make-varexp 'a)
                    (make-varexp 'b)
                    (make-varexp 'c)
                    (make-varexp 'd) 
                    ))
  )

(define test-appexp-2 (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z))))))

(define example-f (make-appexp 'f1 (list (make-varexp 'x))))
;;;---------------------------------------------------------------;;;;;;;;;;;;;;;


;; print-appexp-option-1-top : Exp PosInt -> String 
;; GIVEN: An exp that is an appexp and a posint representing indent
;; RETURNS: A string form of the appexp where it is printed entirely on one line
;; DESIGN STRATEGY: combine simpler functions
;; EXAMPLE: (print-appexp-option-1-top test-appexp 4) returns
;;          "    f1(x, y, f2(z, y), z, a, b, c, d)"))
(define (print-appexp-option-1-top e indent)
  (string-append (make-indent-spaces-func2 indent) (symbol->string (appexp-fn e)) "(" (expressions-printer (appexp-args e)))       
  )

(begin-for-test (check-equal? (print-appexp-option-1-top test-appexp 4) "    f1(x, y, f2(z, y), z, a, b, c, d)"))


                               
;; expressions-printer : ListOfExp -> String
;; GIVEN: A listofexp
;; RETURNS: A String representing the listofexp properly formatted
;; HALTING CONDITION: loe is empty
;; DESIGN STRATEGY: Divide into cases on loe
(define (expressions-printer loe)
  (cond
    [(empty? loe) ")"]
    [(and (empty? (rest loe)) (varexp? (first loe)))
     (string-append (symbol->string (varexp-name (first loe)))
                    (expressions-printer (rest loe))) ]
    [(varexp? (first loe)) (string-append
                            (symbol->string (varexp-name (first loe))) ", "
                            (expressions-printer (rest loe)))]
    [(and (empty? (rest loe)) (appexp? (first loe)))
     (string-append  (symbol->string (appexp-fn (first loe))) "("
                     (expressions-printer (appexp-args (first loe)))
                     (expressions-printer (rest loe)))]
    [(appexp? (first loe)) (string-append  (symbol->string (appexp-fn (first loe)))
                                           "(" (expressions-printer
                                                (appexp-args (first loe))) ", "
                                                 (expressions-printer (rest loe)))]))  


;;;-----------------------------------------------------------


;; print-appexp-option-2-top : Exp PosInt PosInt-> ListOfString
;; GIVEN: An appexp exp, a posint representing the indent, and a posint representing the width
;; RETURNS: A listofstring representing the second possible format for an appexp
;; DESIGN STRATEGY: Combine simpler functions
(define (print-appexp-option-2-top e indent width)
  (cond
    [(list? (print-rest-args (rest (appexp-args e)) (+ indent (string-length (string-append (symbol->string (appexp-fn e)) "(" ))) width))
     (cons (string-append (make-indent-spaces-func2 indent)
                       (symbol->string (appexp-fn e)) "("
                       (print-first-arg (first (appexp-args e))) ",")
        (print-rest-args (rest (appexp-args e)) (+ indent (string-length (string-append (symbol->string (appexp-fn e)) "(" ))) width))]
    [else (cons (string-append (make-indent-spaces-func2 indent)
                       (symbol->string (appexp-fn e)) "("
                       (print-first-arg (first (appexp-args e))) ",")  
        (cons (print-rest-args (rest (appexp-args e)) (+ indent (string-length (string-append (symbol->string (appexp-fn e)) "(" ))) width) '()))] ;condition added because cons was rejecting a non-list
    )       
  )     

;; print-first-arg : exp -> string
;; GIVEN: an exp 
;; RETURNS: the exp in string form
;; DESIGN STRATEGY: Divide into cases on e being a varexp or an appexp
;; Example: (print-first-arg (make-varexp 'z)) returns "z"
(define (print-first-arg e) ;;used for printing the first argument for case 2
  (cond
    [(varexp? e) (symbol->string (varexp-name e))]
    [(appexp? e) (expressions-printer (cons e '()))]
    ) 
  )

;(begin-for-test (check-equal? (print-appexp-option-2-top example-appexp-z 7 20)
;                              (list "       f1(f2(z, y)," "          z)")))
;(begin-for-test (check-equal? (print-first-arg example-appexp-z) "f1(f2(z, y), z)"))
(begin-for-test (check-equal? (print-first-arg (make-varexp 'z)) "z"))

(define example-appexp-z
(make-appexp
  'f1 (list (make-appexp
              'f2 (list (make-varexp 'z)
                        (make-varexp 'y))) 
            (make-varexp 'z))))



;;----------------------------------------;;;


;; print-appexp-option-3-top : Exp PosInt -> ListOfString 
;; GIVEN: An appexp exp and a posint representing the indent
;; RETURNS: A listofstring representing the second possible format for an appexp
#|(define (print-appexp-option-3-top e indent width)
  (cond
    [(list? (print-rest-args (rest (appexp-args e)) (+ indent (string-length (string-append (symbol->string (appexp-fn e)) "(" ))) width))
     (cons (string-append (make-indent-spaces-func2 indent) (symbol->string (appexp-fn e))) (cons (print-first-arg2 (first (appexp-args e)) (+ indent 1)) 
        (print-rest-args (rest (appexp-args e)) (+ indent (string-length (string-append (symbol->string (appexp-fn e)) "(" ))) width)))]
    [else (cons (string-append (make-indent-spaces-func2 indent) (symbol->string (appexp-fn e))) (cons (print-first-arg2 (first (appexp-args e)) (+ indent 1)) 
        (cons (print-rest-args (rest (appexp-args e)) (+ indent (string-length (string-append (symbol->string (appexp-fn e)) "(" ))) width) '())))] ;condition added because cons was rejecting a non-list
  )
  )|#

;; print-appexp-option-3-top : Exp PosInt -> ListOfString 
;; GIVEN: An appexp exp and a posint representing the indent
;; RETURNS: A listofstring representing the second possible format for an appexp
;; DESIGN STRATEGY: Combine simpler functions
(define (print-appexp-option-3-top e indent width)
  (cond
    [(list? (print-rest-args (rest (appexp-args e)) (+ indent (string-length (string-append (symbol->string (appexp-fn e)) "(" ))) width))
     (cons (string-append (make-indent-spaces-func2 indent) (symbol->string (appexp-fn e))) (cons (print-first-arg2 (first (appexp-args e)) (+ indent 1)) 
        (print-rest-args (rest (appexp-args e)) (+ indent 2) width)))]
    [else (cons (string-append (make-indent-spaces-func2 indent) (symbol->string (appexp-fn e))) (cons (print-first-arg2 (first (appexp-args e)) (+ indent 1)) 
        (cons (print-rest-args (rest (appexp-args e)) (+ indent 2) width) '())))] ;condition added because cons was rejecting a non-list
  )
  )

(begin-for-test (check-equal? (print-appexp-option-3-top test-arg2 9 10) (list "         f1" "          (q," "           z)")))


;; print-first-arg2 : Exp PosInt -> ListOfString
;; GIVEN: an exp and a posint representing the indent 
;; RETURNS: the exp in listofstring form
;; DESIGN STRATEGY: Divide into cases on e being a varexp or an appexp
(define (print-first-arg2 e indent) ;;used for printing the first argument for case 3
  (cond
    [(varexp? e) (string-append (make-indent-spaces-func2 indent) "(" (symbol->string (varexp-name e)) ",")]
    ;[(appexp? e) (string-append (make-indent-spaces-func2 indent) "(" (expressions-printer e))]
    [(appexp? e) (string-append (make-indent-spaces-func2 indent) "(" (expressions-printer (cons e '())))]  
    )
  )

(begin-for-test (check-equal? (print-first-arg2 (make-varexp 'z) 10) "          (z,"))
;(begin-for-test (check-equal? (print-first-arg2 test-arg2 10) "          (f1(q, z),"))
(define test-arg2 (make-appexp 'f1 (list (make-varexp 'q) (make-varexp 'z))))

;;----------------------------------------;;;

;; print-rest-args : ListOfExp PosInt PosInt -> ListOfString
;; GIVEN: A listofexp and a posint representing the current indent value
;; RETURNS: A ListOfString representing the listofexp
;; WHERE: Indent is the current indent size, it is altered by nested appexps
;; HALTING CONDITION: (rest loe) is empty
;; DESIGN STRATEGY: Divide into cases on (first loe) being a varexp or an appexp
(define (print-rest-args loe indent width)    
  (cond
    [(empty? loe) ")"] ;will this clause be used?
    [(and (empty? (rest loe)) (varexp? (first loe))) (string-append (make-indent-spaces-func2 indent) (symbol->string (varexp-name (first loe))) ")")] ;option for varexp that is last arg
    [(and (varexp? (first loe)) (not (list? (print-rest-args (rest loe) indent width))))
     (cons (string-append (make-indent-spaces-func2 indent) (symbol->string (varexp-name (first loe))) ",") (cons (print-rest-args (rest loe) indent width) '()))] ;;used when final item in list is a varexp
    [(varexp? (first loe)) (cons (string-append (make-indent-spaces-func2 indent) (symbol->string (varexp-name (first loe))) ",") (print-rest-args (rest loe) indent width))] ;option for varexp 
    [(and (and (empty? (rest loe)) (appexp? (first loe))) (<= (string-length (print-appexp-option-1-top test-appexp indent)) width))    
     (print-appexp-option-1-top test-appexp indent)] ;option for appexp that is last arg and can fit on single line
    [(and (appexp? (first loe)) (<= (string-length (print-appexp-option-1-top test-appexp indent)) width))
     (cons (print-appexp-option-1-top test-appexp indent) (print-rest-args (rest loe) indent width))] ;option for appexp that can fit on a single line
    [(and (and (empty? (rest loe)) (appexp? (first loe)))
          (<= (string-length (string-append (make-indent-spaces-func2 indent) (symbol->string (appexp-fn (first loe))) "(" (print-first-arg (first (appexp-args (first loe)))) ",")) width))
     (print-appexp-option-2-top (first loe) indent width)] ;option for appexp that is last arg and qualifies for option 2
    [(and (appexp? (first loe))
          (<= (string-length (string-append (make-indent-spaces-func2 indent) (symbol->string (appexp-fn (first loe))) "(" (print-first-arg (first (appexp-args (first loe)))) ",")) width))
     (cons (print-appexp-option-2-top (first loe) indent width) (print-rest-args (rest loe) indent width))] ;option for appexp that qualifies for option 2
    [(and (empty? (rest loe)) (appexp? (first loe))) (print-appexp-option-3-top (first loe) indent width)] ;option for appexp that is last arg and must use option 3
    [else (cons (print-appexp-option-3-top (first loe) indent width) (print-rest-args (rest loe) indent width))] ;option for appexp that must use option 3   
    )
  )

(define new_example (make-appexp 'f1 (list (make-varexp 'a) (make-varexp 'b) (make-varexp 'c) (make-varexp 'd) (make-varexp 'e))))






         