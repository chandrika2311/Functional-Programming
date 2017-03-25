#lang racket

(require rackunit)
(require "extras.rkt")
(require "interfaces2.rkt")
(require "SBlock.rkt")
(require "WidgetWorks.rkt")
(require "sets.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide Metatoy%
         )


(define CREATE-BLOCK "b")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Metatoy class

;; Constructor template for Metatoy%
;; A Metatoy is a (new Metatoy% [World StatefulWorld<%>][x Integer][y Integer]
;;                              [sblocks ListOfBlocks])
;; Interpretation: An object of class Metatoy% accepts mesages from big-bang
;; and delivers to the toys sucessfully

;; accepts "b" key event and a new block is added to the existing world
;; gets the world as an init-field

(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field container)
    (init-field x y) ;; the x and y co-ordinates of the location where next new
    ;; block will be created i.e. location of last button-down
    ;; or button-up
    (init-field [listOfSblock empty]) ;; blocks in conatiner
    
    (super-new)
    
    ;; after-tick: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: the metatoy does not change after a tick
    ;; EXAMPLES: (send mt after-tick) --> no change to metatoy mt
    (define/public (after-tick) "no change")
    
    ;; after-drag: Integer Integer -> Void
    ;; GIVEN: the x and y co-ordinates of a location
    ;; EFFECT: the metatoy does not change after a drag
    ;; EXAMPLES: (send mt after-drag) --> no change to metatoy mt
    (define/public (after-drag mx my) "no change")
    
    ;; after-button-down: Integer Integer -> Void
    ;; GIVEN: the x and y co-ordinates of a location
    ;; EFFECT: updates the metatoy to keep the location of the last event
    ;; EXAMPLES: (send mt after-button-down 145 200) -> set x to 145
    ;;                                                  set y to 200
    (define/public (after-button-down mx my)
      (begin
        (set! x mx)
        (set! y my)))
    
    ;; after-button-up: Integer Integer -> Void
    ;; GIVEN: the x and y co-ordinates of a location
    ;; EFFECT: updates the metatoy to keep the location of the last event
    ;; EXAMPLES: (send mt after-button-up 145 200) -> set x to 145
    ;;                                                  set y to 200
    (define/public (after-button-up mx my)
      (begin
        (set! x mx)
        (set! y my)))
    
    ;; -> ListOfToy<%>
    ;; RETURNS : a list of toys stored in Metatoy. 
    (define/public (get-toys)
      listOfSblock)
    
    ;; after-move : NonNegInt NonNegInt -> Metatoy
    ;; GIVEN    : 2 Non Negative Integers indiating mouse position
    ;; EFFECT   : a Metatoy which should follow the MouseEvent
    ;             when there is any other mouse event like drag, button-down,
    ;;            or button-up.
    (define/public (after-move mx my)
      "no change")
    
    
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN:  a key event
    ;; EFFECT: when b is hit a new green slock gets created at specified x and y
    ;;         location
    ;; EXAMPLES: (send sb1 after-key-event "b")-> creates a new green block at
    ;;            the given location or center of the canvas
    ;; STRATEGY: Cases on KeyEvents ke
    (define/public (after-key-event ke)
      (cond
        [(key=? ke CREATE-BLOCK)
         (local ((define new-block (new SBlock% [x x][y y][listOfSblock empty])))
           (begin
             (set! listOfSblock (cons new-block listOfSblock))
             (for-each
              ;; SBlock<%> -> Void
              ;; GIVEN: A Block
              ;; EFFECT: updates the blocks
              (lambda (bk)(send bk update-blocks listOfSblock))listOfSblock)
             (send container add-stateful-widget new-block)
             ))]
        [else "issues with key event block creation"]))
    
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: A Scene
    ;; RETURNS: A Scene like the given one but with SBlock displayed on the
    ;;          scene based on the selected? field
    ;; EXAMPLES: (send sb1
    ;;              add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))->
    ;;             given scene is retruned
    (define/public (add-to-scene sc)sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTING

(begin-for-test
  (local
    ((define mt  (new Metatoy% [container (container-init 500 600)]
                      [x (/ 500 2)]
                      [y (/ 600 2)]))
      )
     (send mt after-tick)
     (send mt after-key-event "b")
     (send mt get-toys)
     (send mt after-button-down 300 100)
     (send mt after-drag 30 10)
     (send mt after-button-up 300 100)
     (send mt after-move 30 10)
     (send mt add-to-scene (empty-scene 500 600))
     (send mt after-key-event "b")
   
     (send mt after-key-event "x")))