#lang racket

(require rackunit)
(require "extras.rkt")
(require "interfaces2.rkt")
(require "sets.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define SQUARE-SIDE 20)
(define SQUARE-TYPE "outline")
(define SQAURE-SELECT-COLOR "red")
(define SQAURE-UNSELECT-COLOR "green")
(define INITIAL-STATE 0)

(provide SBlock%
         make-block
       )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A SBLOCK is a (new SBLOCK% [x Integer][y Integer][selected? Booelan]
;;                            [drag? Boolean][listOfSblock ListofSBlocks<%>]
;;                            [team ListofSBlocks<%>]
;;                            [saved-mx Integer][saved-my Integer])

;; INTERPRETATION:
;; x represents the x-coordinates of the center of SBlock
;; y represents the y-coordinates of the center of SBlock
;; selected? respresents if the SBlock is selected by mouse or not
;;           and is optional field
;; drag? respresents if the SBlock<%> is being dragged by mouse or not
;;        and is optional field
;; team represents the list of SBlocks<%> which are all in a team
;;      and is optional field
;; listOfSblock represents the list of all SBlocks<%> in the conatiner
;;      and is optional field
;; saved-mx represents the x-coordinates of the mouse pointer
;;         and is optional field
;; saved-my represents the y-coordinates of the mouse pointer
;;         and is optional field
(define SBlock%
  (class* object% (SBlock<%>)
    
    ;; the x and y position of the center of SBlock<%>
    ;; list of all the SBlock<%> in the metatoy
    (init-field x y listOfSblock)
    
    ;; if the SBlock<%> should be dragged
    (init-field [drag? false])
    
    ;; if the SBlock<%> is selectd
    (init-field [selected? false])
    
    
    
    ;; team of the this SBlock
    (init-field [team (list this)])
    
    ;; the relative position of the x and y co-ordinate of the SBlock which
    ;; respect to the Sblock's center co-ordinates and the current mouse pointer
    ;; if the Sblock is selected, else any value.Default value is 0.
    (init-field [saved-mx INITIAL-STATE][saved-my INITIAL-STATE])
    
    ;; images of the Sblocks to be dispalyed based on selected? 
    (field [SQUARE (square SQUARE-SIDE SQUARE-TYPE SQAURE-UNSELECT-COLOR)])
    (field [SQUARE-SELECTED (square SQUARE-SIDE SQUARE-TYPE SQAURE-SELECT-COLOR)])
    
    (super-new)
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: A Scene
    ;; RETURNS:: A Scene like the given one but with SBlock displayed on the
    ;;          scene based on the selected? field
    ;; EXAMPLES: (send sb1
    ;;              add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))->
    ;;            displays a red sblock if the selected? is true else will
    ;;            display a green sblock
    ;; STRATEGY: Cases on if the SBlock is selected
    (define/public (add-to-scene scene)
      (if selected?
          (place-image SQUARE-SELECTED x y scene)
          (place-image SQUARE x y scene))) 
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: the block does not change after a tick
    ;; EXAMPLES: (send sb1 after-tick) -> sb1 without any change
    (define/public (after-tick)
      "after-tick")
    
    ;; after-button-down: NonNegInt NonNegInt-> Void
    ;; GIVEN: the x and y co-ordiantes of the current mouse pointer
    ;; EFFECTS: update the relative location of the SBlock center absed on if
    ;;          the mouse pointer is inside the SBlock or not and change the
    ;;          selected? to true
    ;; EXAMPLES: (send sb1 after-button-down 201 305)->
    ;;             updates sb1 selected?  to true and  saved-mx andd saved-my
    ;;             to 1 and 5
    ;; STRATEGY:Cases on if the mouse pointer is inside the Sblock or not
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))
            (for-each
             ;; SBlock<%>->Void
             ;; GIVEN: A SBlock from the team
             ;; EFFECT: Modifies the block that is to be selected
             (lambda (b)(send b update-drag true))
             team))
          (begin
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))))) 
    
    ;; update-drag: Booelan -> Void
    ;; GIVEN: state of the Sblock with respect to drag represnted by boolean
    ;; EFFECTS: set the drag? field for the SBlock to given value indicating
    ;;          if the sblock can be dragged or not
    (define/public (update-drag x)
      (set! drag? x))
    
    ;; in-square?: NonNegInt NonNegInt -> Boolean
    ;; GIVEN: x and y co-ordiantes of a location on canvas
    ;; RETURNS: : true if givenx and y co-ordiantes are inside this block
    ;; EXAMPLES: if sb1 at 50 50 
    ;;          (in-square? 45 55)-> true
    ;; STRATEGY: Combine simpler functions
    (define (in-square? x1 y1 )
      (and (<= (abs (- x1 x))(/ SQUARE-SIDE 2))
           (<= (abs (- y1 y))(/ SQUARE-SIDE 2)))) 
    
    ;; after-drag:  NonNegInt NonNegInt -> Void
    ;; GIVEN:  and y co-ordiantes of a mouse location on canvas
    ;; EFFECTS: if sblock is selected then set the x and y
    ;;          coordnates of the SBlock to relative value of saved-mx
    ;;          and saved-my and current mouse coordinates
    ;; EXAMPLES: (send sb1 after-drag 400 450)-> id sb1 is selected then sb1
    ;;            will be dragged to x=400 and y=450 and while being
    ;;            dragged if it intersects other block that will be dragged too
    ;; STRATEGY:Cases on if the mouse pointer is inside the Sblock or not
    (define/public (after-drag mx my)
      (cond
        [selected? (begin
                     (set! x (- mx saved-mx ))
                     (set! y (- my saved-my ))
                     (for-each
                      ;; SBlock<%>->Void
                      ;; GIVEN: A SBlock from the team
                      ;; EFFECT: if sblock intersects adds the block
                      ;;          to the team
                      (lambda (bk)(gather-team bk mx my))
                      (set-diff listOfSblock team)))]
        [drag?
         (begin
           (set! x (- mx saved-mx ))
           (set! y (- my saved-my )))]
        [else "no change"]))
    

   ;; gather-team: SBlock<%> NonNegInt NonNegInt -> Void
   ;; GIVEN: A SBlock<%> and a location on the canvas
   ;; EFFECTS: If the SBlock and the location intersects then add the sblock to
    ;;         this sblock's  team and update new teammamte so it will be
    ;;         dragged too
    ;; EXAMPLES: (gather-teammate sb1 255 305)-> here it isntersects hence the
    ;;            new block will be added to teama nd be updated nad dragged
    ;; STRATEGY: Cases on if the SBlock<%> block-intersect?
    (define (gather-team bk mx my)
      (if (block-intersect? bk)
          (let ((team1 team))
            (begin (add-teammate bk)
                   (for-each
                    ;; SBlock<%>->Void
                    ;; GIVEN: A SBlock from the world
                    ;; EFFECT: modifies its mx my and drag to current
                    ;;         co-ordinates suitable for drag
                    (lambda (cur-bk)
                      (send cur-bk update-mx mx)
                      (send cur-bk update-my my)
                      (send cur-bk update-drag true))
                    (set-diff team team1))))"no intersection")) 
    

    ;; block-intersect? SBlcok<%> -> Boolean
    ;; GIVEN: a sblock
    ;; RETURNS: : true if given block intersects this block
    ;; EXAMPLES: if sb1 at 50 50 and sb2 at 55, 65 then
    ;;          (block-intersect? sb2)-> true
    ;; STRATEGY: Combine simpler functions
    (define (block-intersect? bk)
      (and (<= (abs (- x (send bk sblock-x))) SQUARE-SIDE)
           (<= (abs (- y (send bk sblock-y))) SQUARE-SIDE)))
    
    
    
    ;; after-button-up: Integer Integer -> Void
    ;; GIVEN: the x and y co-ordinates of a location
    ;; EFFECT: updates the SBlock to not be dragged or selected
    ;; EXAMPLES: (send sb1 after-button-up 145 200) ->
    ;;            the selected? and drag? set to false for sb1
    (define/public (after-button-up mx my)
      (begin
        (set! selected? false)
        (set! drag? false))) 

   ;; after-key-event : KeyEvent -> Void
   ;; GIVEN: Key event to be performed
   ;; EFFECT: the block does not change after a key event
   ;; EXAMPLES: (send sb1 after-key-event "s") -> sb1 without any change    
   (define/public (after-key-event ke)
      "no change")

   ;; after-move : NonNegInt NonNegInt -> Void
   ;; GIVEN: teh x and y coordinates of the location on the canvas
   ;; EFFECT: the block does not change after move
   ;; EXAMPLES: (send sb1 after-key-event "s") -> sb1 without any change     
    (define/public (after-move mx my) this)

   ;; get-team : -> ListOfBlock<%>
   ;; RETURNS: the teammates of this block
   ;; EXAMPLES: (send sb1 get-team )-> get list of sb1's team
   ;; STRATEGY: Combine simpler fucntions
    (define/public (get-team)
      (set-diff team (list this)))

    ;; add-teammate: Block<%> -> Void
    ;; EFFECT: adds the given block to this block's team
    ;; EXAMPLES: (send sb1 add-teammate sb2)-> adds sb2 to sb1's temmates list
    ;; SRTATEGY: USe HOF lambda on  new-team
    (define/public (add-teammate bk)
      (let ((new-team (set-union team (cons bk (send bk get-team)))))
        (for-each
         ;; SBlock<%>->Void
         ;; GIVEN: A SBlock from the world
         ;; EFFECT: modifies the current block's team and addes the new block
         (lambda (b)(send b recognize-team new-team))new-team))) 

   ;; recognize-team : ListOfBlock<%> -> Void
   ;; GIVEN : the teammates of this block
   ;; EFFECTS: updates the block with its existing teammate blocks
   ;; EXAMPLES: (send sb1 recognize-team )-> get list of sb1's team
   ;; STRATEGY: Combine simpler fucntions
    (define/public (recognize-team tm)(set! team tm))

   ;; recognize-sblock : ListOfBlock<%> -> Void
   ;; GIVEN : the SBlocks<%> in the container
   ;; EFFECTS: updates the block with its existing blocks in the conatiner
   ;; EXAMPLES: (send sb1 recognize-sblock (list sb3 sb2) )-> updates the
   ;;            sb1 with other blcok ins the world which are sb2, sb3
   ;; STRATEGY: Combine simpler fucntions 
    (define/public (recognize-sblock sbk)(set! listOfSblock sbk))

    ;; sblock-x : -> Integer
    ;; RETURNS:  the x coordiante of the sblock
    ;; EXAMPLES: (send sb1 sblock-x)->250
    (define/public (sblock-x) x)  

    ;; sblock-y : -> Integer
    ;; RETURNS:  the x coordiante of the sblock
    ;; EXAMPLES: (send sb1 sblock-y)->300
    (define/public (sblock-y) y)

    ;; update-mx: Integer -> Void
    ;; GIVEN: An integer
    ;; RETURNS: sets the saved-mx of the block to current mx of the mouse pointer
    ;; STRATEGY: Combine simpler fucntions 
    (define/public (update-mx mx) (set! saved-mx (- mx x)))

    ;; update-mx: Integer -> Void
    ;; GIVEN: An integer
    ;; RETURNS: sets the saved-mx of the block to current mx of the mouse pointer
    ;; STRATEGY: Combine simpler fucntions 
    (define/public (update-my my) (set! saved-my (- my y)))
    
    ;; update-blocks: ListOfBlock<%> -> Void
    ;; GIVEN: Blocks (ListofBlock<%> in the world)
    ;; EFFECT: updates the block's idea of the blocks that exist in the world
    ;; EXAMPLE: (send sb6 update-blocks (list sb6 sb7)) -> Updates sb6's blocks
    ;;                                                      with the given blocks
    ;; STRATEGY: Combine simpler fucntions 
    (define/public (update-blocks b) (set! listOfSblock b))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-block : NonNegInt NonNegInt ListOfSBlock -> SBlock
;; GIVEN: an x and y position, and a list of blocks
;; WHERE: the list of blocks is the list of blocks already on the playground.
;; RETURNS: a new block, at the given position, with no teammates
;; NOTE: it is up to you as to whether you use the third argument or
;; not.  Some implementations may use the third argument; others may not.
;; EXAMPLES:
;; STRATEGY:
(define (make-block x y listOfSblock)
  (new SBlock% [x x][y y] [listOfSblock listOfSblock ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTING

(begin-for-test
  (local
    ((define sb1 (make-block 250 300 empty)) 
     (define sb2 (make-block 200 201 empty))
     (define sb3 (make-block 201 202 empty))
     (define sb4 (make-block 203 204 empty))
     (define sb5 (make-block 300 250 empty))
      (define sb6 (make-block 310 260 empty))
     (define sb7 (make-block 110 110 empty))
     (define sb8 (make-block 30 30 empty))
     (define sb9 (new SBlock% [x 20][y 30][listOfSblock (list sb1 sb2)])))
    
    (send sb1 update-blocks (list sb1 sb2 sb3))
    (send sb2 update-blocks (list sb1 sb2 sb3))
    (send sb3 update-blocks (list sb1 sb2 sb3)) 
    (send sb1 after-tick)
    (send sb1 after-move 200 250)
    (send sb1 after-key-event "x")
    (check-equal? (send sb1 sblock-x) 250 " 250 x co-ordiante")
    (check-equal? (send sb1 sblock-y) 300 " 250 y co-ordiante")
    
    (send sb3 after-button-down 20 30)
    (send sb3 after-drag 80 70)
    (send sb3 after-button-up 80 70)
    (check-equal? (send sb3 sblock-x) 201 "201 x co-ordiante")
    (check-equal? (send sb3 sblock-y) 202 " 202 y co-ordiante")
    
    (send sb3 after-button-down 75 100)
    (send sb3 after-drag 65 80)
    (send sb1 after-button-up 65 80)
    (check-equal? (length (send sb3 get-team)) 0 " # team member")
    (check-equal? (length (send sb2 get-team)) 0 "# team member")
    (check-equal? (send sb3 sblock-x) 201 " x coordinate")
    
    (send sb2 add-teammate sb4 )
    (send sb2 add-teammate sb3 )
     (send sb2 update-drag true)
    (send sb2 after-drag 206 208)
    (check-equal? (send sb2 sblock-x) 206 " 206 x co-ordiante")
    (check-equal? (send sb2 sblock-y) 208 "  y co-ordiante")
    (send sb2 after-button-down 206 208)
     (send sb2 after-drag 206 208)
    (send sb2 after-button-up 206 208)
    (check-equal? (length (send sb2 get-team)) 2 "# team member")
    (send sb2 after-drag 430 350)
    (send sb1 update-drag true)
    (send sb1 after-drag 251 302)
    
    (send sb1 update-drag true)
    (send sb1 after-button-down 251 301)
    (send sb1 add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
    (send sb1 after-drag 150 150)
    (send sb1 after-button-up 150 150)
    (check-equal? (send sb1 sblock-x) 150 "  x coordinate")
    (check-equal? (length (send sb1 get-team)) 0 "# team member")

    (send sb5 update-blocks (list sb1 sb2 sb3 sb4))
    (send sb5 after-tick)
    (send sb5 after-key-event "s")
    (send sb5 after-button-down 300 250)
    (send sb5 after-drag 400 400)
    (send sb5 after-button-up 400 400)
    (send sb5 after-button-down 500 500)
    (send sb5 after-drag 101 101)
    (send sb5 after-button-up 101 101)
    (send sb5 update-drag true)
    (send sb5 after-button-down 201 201)
    (send sb5 add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
    (send sb5 after-drag 150 150)
    (send sb5 after-button-up 150 150)
    (check-equal? (send sb5 sblock-x) 349 "  x coordinate")
    (check-equal? (length (send sb5 get-team)) 0 "# team member")
    (check-equal? (length (send sb2 get-team)) 2 "# team member")
    (check-equal? (send sb2 sblock-x) 206 "team length")
    (send sb2 after-drag 200 200)
    (send sb2 get-team)

    (send sb6 update-blocks (list sb6 sb7 sb8))
    (send sb7 update-blocks (list sb6 sb7 sb8))
    (send sb8 update-blocks (list sb6 sb7 sb8))
    
    (check-equal? (send sb6 sblock-x) 310 "  x coordinate")
    (check-equal? (send sb6 sblock-y) 260 "  y coordinate")
    (send sb6 after-tick)
    (send sb6 after-key-event "x")
    
    (send sb6 after-button-down 310 260)
    (send sb6 after-drag 410 410)
    (send sb6 after-button-up 410 410)
    (check-equal? (send sb6 sblock-x) 410 "  x coordinate")
    (check-equal? (send sb6 sblock-y) 410 "  y coordinate")
  
    (send sb6 after-button-down 510 510)
    (send sb6 after-drag 310 310)
    (send sb6 after-button-up 310 310)
    (check-equal? (send sb6 sblock-x) 410 "  x coordinate")
    (check-equal? (send sb6 sblock-y) 410 "  y coordinate")
    
    (send sb6 after-button-down 410 410)
    (send sb6 after-drag 111 111)
    (send sb6 after-button-up 111 111)
    (check-equal? (length (send sb6 get-team)) 1 "# team member")
    (check-equal? (length (send sb7 get-team)) 1 "# team member")
    (check-equal? (send sb6 sblock-x) 111 "  x coordinate")
    
    (send sb8 after-button-down 30 30)
    (send sb8 add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
    (send sb8 after-drag 91 111)
    (send sb8 after-button-up 91 111)
    (check-equal? (length (send sb8 get-team)) 2 "# team member")
    (check-equal? (length (send sb7 get-team)) 2 "# team member")

    (send sb6 update-drag true)
    (send sb6 after-button-down 211 211)
    (send sb6 add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
    (send sb6 after-drag 160 160)
    (send sb6 after-button-up 160 160)
    (check-equal? (send sb6 sblock-x) 60 "  x coordinate")

    (check-equal? (length (send sb6 get-team)) 2 "# team member")


    (send sb1 recognize-sblock sb4)
    
    ))    
