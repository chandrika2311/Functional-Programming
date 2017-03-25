;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(provide image-area)
(check-location "01" "Q3.rkt")

;;Purpose of the program is to define the function image-area, which counts the number of pixels in a given image.
;;-------------------------------------------------------------------------------------------------------------------------------------------
;;DATA DEFINITION: None, Only function definition as no information analysis or data design

;-------------------------------------------Contract, Purpose statement and Examples
;; image-area: image -> number in (numberOfpixels)
;; GIVEN:  Image
;; RETURN: Number of Pixels

;; EXAMPLES:
;;          (image-area star1) => 342
;;          (image-area circle1) => 144

;; DESIGN STRATEGY: Function composition

(define (image-area img)
  ( * (image-height img) (image-width img)))
;-------------------------------------------Test Cases
;; TESTS
(begin-for-test       
(check-equal?(image-area (star 12 "solid" "green")) 342
              "Area of star should be 342")
(check-equal?(image-area (circle 6 "solid" "green")) 144
              "Area of circle should be 144"))
