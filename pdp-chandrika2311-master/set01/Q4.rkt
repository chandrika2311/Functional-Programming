;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require racket/string)
(provide string-insert )
(check-location "01" "Q4.rkt")
 
;; Start Of Problem Set:
;; Define the function string-insert, which consumes a string str plus a number i and inserts "_" at the ith position of str.
;; Assume i is a number between 0 and the length of the given string (inclusive).
;;-------------------------------------------------------------------------------------------------------------------------------------------
;; DATA DEFINITIONS:None, Only function definition as no information analysis or data design

;;-------------------------------------------Contract, Purpose statement and Examples
;; string-insert: String nonNeginteger Char -> String
;; GIVEN: Str: String,
;;        i  : nonNeginteger(Position in string between 0 and length of string(inclusive))

;; RETURNS: String with '_' on position i of input string

;; EXAMPLES    : (string-insert "Chandrika" 2) = "Ch_andrika"
;;             : (string-insert "Aditya" 4) = "Adi_tya"
;; DESIGN STRATEGY: Combine simpler functions

(define ( string-insert str i)
  (string-append
   (substring str 0 i)
   "_"
   (substring str i)
   ))

;-------------------------------------------Test Cases
;; TESTS
(begin-for-test
  (check-equal? (string-insert "Chandrika" 9)"Chandrika_"
    " Expected result is Ch_andrika")
 (check-equal? (string-insert "Aditya" 4)"Adit_ya"
   "Expected result is Adit_ya")
)
