;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require racket/string)
(provide string-delete)
(check-location "01" "Q5.rkt")


;;Purpose:Define the function string-delete, which consumes a string plus a number i
;;        and deletes the ith position from str.
;;        Assume i is a number between 0 (inclusive)
;;        and the length of the given string (exclusive)
;;-------------------------------------------------------------------------------------------------------------------------------------------

;;DATA DEFINITION: None, Only function definition as no information analysis or data design

;;-------------------------------------------Contract, Purpose statement and Examples
;; string-delete:String nonNeginteger -> String

;; GIVEN: Str: String,
;;        i  : nonNeginteger(Position in string between 0(INCLUSIVE) and length of string(EXCLUSIVE))
;; RETURNS: String without character on position i of input string

;; EXAMPLES    : (string-delete "Chandrika" 2) = "Chndrika"
;;             : (string-delete "Aditya" 4) = "Adita"
;; DESIGN STRATEGY: Combine simpler functions

(define ( string-delete str i)
  (string-append
   (substring str 0 i)
   (substring str (+ i 1))
   ))

;-------------------------------------------Test Cases

;; TESTS
(begin-for-test
  (check-equal? (string-delete "Chandrika" 8 )"Chandrik"
    " Expected result is Chandria")
 (check-equal? (string-delete "Aditya" 4)"Adita"
   "Expected result is Adita")
 (check-equal? (string-delete "Aditya" 0)"ditya"
   "Expected result is ditya")
)


