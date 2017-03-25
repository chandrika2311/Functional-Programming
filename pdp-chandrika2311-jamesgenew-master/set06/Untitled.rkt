;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

;;list of numbers is one of
;;-- number
;;-- lon


;; diff : NonEmptyListOfNumber -> Number
;; GIVEN: a nonempty list of numbers
;; RETURNS: the result of subtracting the numbers, from left to right.
;; EXAMPLE:
;; (diff (list 10 5 3)) = 2

(define(diff lon)
  (calculate_number_difference lon))

(define (calculate_number_difference lon)
  (+ (first lon) (calculate_numbers_difference (rest lon))))

(define(calculate_numbers_difference lon)
  (+(first lon)(rest lon)))
                        