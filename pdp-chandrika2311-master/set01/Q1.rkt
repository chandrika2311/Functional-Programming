;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide disFrmOrigin)
(check-location "01" "Q1.rkt")
 
;;Purpose of this program is to define a function that consumes two numbers, x and y, and that computes the distance of point (x,y) to the origin.
;-----------------------------------------------------------------------------------------------------------------------------------------------
;; DATA DEFINITIONS: None, Only function definition as no information analysis or data design

;-------------------------------------------Contract, Purpose statement and Examples
;; disFrmOrigin: Real Real -> Real
;; GIVEN       : 2 Numbers constituting to point(x,y) on a plain
;; RETURNS     : Distance of the point from origin (0, 0) in centimeters
;; EXAMPLES    :(disFrmOrigin 3 2) = #3.6055
;;             : (disFrmOrigin 1 2) = #2.236 
 
(define (disFrmOrigin x y)
  ( sqrt (+(* x x) (* y y))))
;-------------------------------------------Test Cases
;;TEST
(begin-for-test
  (check-equal? (disFrmOrigin 3 2) #i3.605551275463989
                "Distance from origin of p(3, 2) should be #i3.605551275463989")

  (check-equal? (disFrmOrigin 1 2) #i2.23606797749979
                "Distance from origin of p(1, 2) should be #i2.23606797749979")  )            
                
