;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide string-last)
(check-location "01" "Q2.rkt")

;;Purpose of this program is to define the function string-last, which extracts the last 1String(String of length 1) from a non-empty string.
;;-------------------------------------------------------------------------------------------------------------------------------------------
;;DATA DEFINITION: None, Only function definition as no information analysis or data design

;;-------------------------------------------Contract, Purpose statement and Examples
;; string-last: String -> 1String(String with length 1)
;; GIVEN  : String of an arbitrary length
;; RETURN : Returns last character(String with length 1) of the inout string

;;EXAMPLES:
;;   (string-last "Hello") => "o"
;;   (string-last "Olla!!") => "!"

;;DESIGN STRATEGY: Extract last character of string

(define (string-last strng)
  (substring strng (-(string-length strng) 1)))

;-------------------------------------------Test Cases
;; TESTS
(begin-for-test
  (check-equal? (string-last "Hello") "o" 
    "Last character of Hello should be 'o' ")
  (check-equal? (string-last "Olla!!") "!" 
    "Last character of Olla!! should be '!' "))




