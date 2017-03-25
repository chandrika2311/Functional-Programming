;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide initial-world
         run
         world-after-mouse-event
         world-after-key-event
         world-to-trees
         tree-to-root
         tree-to-sons
         node-to-center )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 2htdp/universe)
(require 2htdp/image)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ON-SCREEN FUNCTION.

;;on-screen : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; blank canvas
;; RETURNS: the final state of the world
(define (run initial-pos)
  (big-bang (initial-world initial-pos)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CIRCLE-RADIUS 20)
(define TOP-X(/ CANVAS-WIDTH 2))
(define TOP-Y CIRCLE-RADIUS)
(define GREEN-UNSELECTED-CIRCLE (circle CIRCLE-RADIUS "outline" "green"))
(define GREEN-UNSELECTED-SQUARE (square (* 2 CIRCLE-RADIUS) "outline" "green"))
(define GREEN-SELECTED-CIRCLE (circle CIRCLE-RADIUS "solid" "green"))
(define GREEN-SELECTED-SQUARE (square (* 2 CIRCLE-RADIUS) "solid" "green"))
(define CIRCLE "circle")
(define SQUARE "square")

;;__________________________________________________________________
;..................DATA-DEFINITIONS.................................
;;__________________________________________________________________
(define-struct world(lot))
;; A World is a (make-world lot)
;; Interpretation: World is a list of Tree
;; lot: list of tree
;; TEMPLATE
;; world-fn : World -> ??
;;(define (world-fn w)
;;  (... (worldstate-lot w))
;_____________________________________________________
;;A Shape is one of
;-- "square"
;-- "circle"
;INTERPRETATION: Shape is one of square or circle
;;shape-fn : Shape -> ??
;(define (shape-fn shape)
;  (cond
;    [(string=? shape "square") ...]
;    [(string=? shape "circle") ...]
;    [else ...]))
;_____________________________________________________
(define-struct node (shape selected? x y dx dy))
#|
A node is (make-node circle? square? selected? x y dx dy)
Interpretation:
--shape: shape of node, Square or Circle
--selected?: true iff node selected by mouse action
--x, y : centre of node(square/circle)
--dx, dy : difference between circle centre and mouse point

|#
;_____________________________________________________
(define-struct tree(node children))
#|
Interpretation:
A Tree is a (make-tree node trees)
node: Node 
Trees: A Trees is one of: 
      -- empty
      -- (cons tree trees)
__________________
tree-fn: Tree ->??
HALTING MEASURE: number of children in t
(define (tree-fn t)
  (... (tree-node t) 
       (trees-fn (tree-children t))))

;; trees-fn : trees -> ??
(define (Trees-fn ts)
  (cond
    [(empty? tss) ...]
    [else (... (tree-fn (first ts))
               (trees-fn (rest ts)))]))
|#
;;__________________________________________________________________
;;                        END OF DEFINITIONS
;;__________________________________________________________________
;tree-to-root : Tree -> Node
;GIVEN: a tree
;RETURNS: the node at the root of the tree
;;EXAMPLES:(tree-to-root tree1)=>(make-node "circle" #t 400 370 0 0)
;;STRATEGY:Use Template For Node on t
(define (tree-to-root t)
  (tree-node t))
(begin-for-test(check-equal? (tree-to-root tree1)(make-node "circle" #t 400 370 0 0)))
;;__________________________________________________________________
;tree-to-sons : Tree -> ListOfTree
;GIVEN: a tree
;RETURNS: the data associated with the immediate subtrees of the given tree.
;;EXAMPLES:(tree-to-sons tree3-mouse)=>(list(make-tree (make-node "circle" #f 20 40 0 0) empty)
                             ;(make-tree (make-node "circle" #f 10 40 0 0) empty)
                             ;)
;;STRATEGY:Use Template For Node on t
(define(tree-to-sons t)
  (tree-children t))
(begin-for-test(check-equal?(tree-to-sons tree3-mouse)
                            (list(make-tree (make-node "circle" #f 20 40 0 0) empty)
                             (make-tree (make-node "circle" #f 10 40 0 0) empty)
                             )))

;;__________________________________________________________________
;;world-to-trees : World -> ListOfTree
;;GIVEN: a World
;RETURNS: a list of all the trees in the given world.
;STRATEGY:Use Template for World on w
;EXAMPLES:(world-to-trees (make-world world-unselected1))=>
#|
(list(make-tree(make-node "circle" #f 40 37 0 0)
                 (list(make-tree (make-node "circle" #f 20 40 0 0)
                                 (list(make-tree (make-node "circle" #f 21 40 0 0)empty)
                                      (make-tree (make-node "circle" #f 22 40 0 0) empty)))
                      (make-tree (make-node "circle" #f 30 40 0 0)
                                 (list(make-tree (make-node "circle" #f 31 40 0 0)empty)
                                      (make-tree (make-node "circle" #f 32 40 0 0) empty)))
                      )))
|#
(define (world-to-trees w)
  (world-lot w))
(begin-for-test
  (check-equal?(world-to-trees (make-world world-unselected1))
               (list(make-tree(make-node "circle" #f 40 37 0 0)
                 (list(make-tree (make-node "circle" #f 20 40 0 0)
                                 (list(make-tree (make-node "circle" #f 21 40 0 0)empty)
                                      (make-tree (make-node "circle" #f 22 40 0 0) empty)))
                      (make-tree (make-node "circle" #f 30 40 0 0)
                                 (list(make-tree (make-node "circle" #f 31 40 0 0)empty)
                                      (make-tree (make-node "circle" #f 32 40 0 0) empty)))
                      )))))
;;__________________________________________________________________
;node-to-center : Node -> Posn
;GIVEN: Node
;RETURNS: the center of the given node as it is to be displayed on the
;scene.
;EXAMPLES:(node-to-centre (make-node "circle" #f 20 40 0 0))=>(make-posn 20 40)
;STRATEGY:Use template for Node on n

(define (node-to-center n)
  (make-posn (node-x n)(node-y n)))
(begin-for-test(check-equal?(node-to-center (make-node "circle" #f 20 40 0 0))(make-posn 20 40)))
;;__________________________________________________________________
;Functions related to world:
;;initial-world: any-> 
;; GIVEN: any value (ignored)
;; RETURNS: the initial world where canvas is empty
;; STRATEGY : Use of constructor on world
;; EXAMLPLES:
;; (initial-world 2) => (make-world empty)
;; (initial-world 3) => (make-worlds empty)
(define (initial-world x)
 (make-world empty))
;;______Tests
(begin-for-test(check-equal?(initial-world 4)(make-world empty)))
;;__________________________________________________________________
;; is-c-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a c instruction
;;STRATEGY: Divide on cases on key event
(define (is-c-key-event? kev)
  (key=? kev "c"))
;;__________________________________________________________________
;; is-s-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a s instruction
;;STRATEGY: Divide on cases on key event
(define (is-s-key-event? kev)
  (key=? kev "s"))
;;__________________________________________________________________
;; is-d-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a d instruction
;;STRATEGY: Divide on cases on key event
(define (is-d-key-event? kev)
  (key=? kev "d")) 

;;__________________________________________________________________
;World-after-key-event
;;__________________________________________________________________
;;__________________________________________________________________
;;__________________________________________________________________
;;__________________________________________________________________
;world-after-key-event : World KeyEvent -> World
;GIVEN: a World and a key event
;RETURNS: the state of the world as it should be following the given key event
;;EXAMPLES:(world-after-key-event (make-world (list tree9)) "c")=>(make-world (list tree10))
;;(world-after-key-event (make-world (list tree9)) "s")=>(make-world (list tree10-square))
;;STRATEGY:Cases for key event on w
(define (world-after-key-event w kev) 
  (cond
    [(is-c-key-event? kev)(create-new-tree-in-world w CIRCLE)]
    [(is-s-key-event? kev)(create-new-tree-in-world w SQUARE)]
    [(is-d-key-event? kev)(remove-tree-from-world w)]
    [else w] ))
(begin-for-test(check-equal?
                (world-after-key-event (make-world (list tree9)) "c")(make-world (list tree10)))
               (check-equal?
                (world-after-key-event (make-world (list tree9)) "s")
                (make-world (list tree10-square)))
               (check-equal?(world-after-key-event(make-world world-root-removal1) "d")
                            (make-world world-after-root-removal2))
               (check-equal?(world-after-key-event(make-world world-root-removal1) "e")
                            (make-world world-root-removal1)))
;;__________________________________________________________________
;;__________________________________________________________________
;;__________________________________________________________________
;;__________________________________________________________________
;;create-new-tree-in-world w -> w
;;GIVEN: world with list of tree
;;RETURN:World with node added iff node is selected and c pressed
;;EXAMPLES: (create-new-tree-in-world(make-world (list tree9)) CIRCLE)=>(make-world (list tree10))
;;STRATEGY: Use HOF Foldr on lot
(define (create-new-tree-in-world w shape)
  (cond
    [(check-world-selected? w)
     ;;given: tree
     ;;Returns:list of trees
     (make-world(foldr(lambda(w1 wl) (cons(creating-new-tree-fn w1 shape) wl )) empty (world-lot w)))]
    [else (make-world (cons(make-tree (make-node shape #f TOP-X TOP-Y 0 0)'())(world-lot w)))]))
;---Test Cases
(begin-for-test(check-equal?(create-new-tree-in-world(make-world (list tree9)) CIRCLE)
                            (make-world (list tree10)))
               (check-equal?(create-new-tree-in-world(make-world (list tree-unselected)) CIRCLE)
                            (make-world (list (make-tree (make-node CIRCLE #f TOP-X TOP-Y 0 0)'())
                                              tree-unselected )))
               (check-equal?(create-new-tree-in-world(make-world (list tree9)) SQUARE)
                            (make-world (list tree10-square))))
;;__________________________________________________________________
;;remove-tree-from-world: w-> w
;;GIVEN: world
;;RETURNS: World with selected node removed
;;EXAMPLES:
;(remove-tree-from-world (make-world world-root-removal1))=>(make-world world-after-root-removal2)
;;STRATEGY: Use HOF Foldr on (world-lot w)
(define (remove-tree-from-world w)
  (cond
    [(check-world-selected? w)
     ;;GIVEN:tree with selected nodes
     ;;RETURNS: tree with nodes of the children of selected nodes and unselected children of root
     (make-world(foldr(lambda(w1 wl) (if (node-selected? (tree-node w1))
                                         (append(handle-root-node-deletion w1) wl)
                                         (cons(remove-tree-from-t-fn w1) wl )))
                      empty (world-lot w)))]
    [else w]))
;-----Tests
(begin-for-test
  (check-equal?(remove-tree-from-world (make-world world-root-removal1))
               (make-world world-after-root-removal2))
  (check-equal?(remove-tree-from-world (make-world world-removal1))
               (make-world world-removal2))
  (check-equal?(remove-tree-from-world (make-world world-unselected1))
               (make-world world-unselected1)))

;;__________________________________________________________________
;;handle-root-node-deletion: Tree -> trees(list of trees)
;;GIVEN: A Tree
;;RETURN: returns children of the root node during deletion of the root, as all the children of root
; nodes should become differennt trees
;;EXAMPLES:(handle-root-node-deletion tree4)=>(list(make-tree(make-node "circle" #f 400 430 0 0)'()))
;;STRATEGY:Use template for Tree on t
(define(handle-root-node-deletion t)
  (tree-children t))
;Tests
(begin-for-test(check-equal?(handle-root-node-deletion tree4)
                            (list(make-tree(make-node "circle" #f 400 430 0 0)'()))))
;;__________________________________________________________________
;;remove-tree-from-t-fn : tree -> tree
;;GIVEN: Tree
;RETURNS: Tree after removing the selected tree node from the tree and adding its children to the
;parent
;;EXAMPLE:(remove-tree-from-t-fn tree10-square)=>tree-10-sq-for-remove1
;;(remove-tree-from-t-fn tree10-deeper-selection)=>tree10-deeper-removal1
;;STRATEGY: Use template of tree on t
(define (remove-tree-from-t-fn t)
  (if(is-child-selected? t)                          ;checking if parent has any selected children
     (make-tree(tree-node t)(append(get-children-of-selected-node(get-child-selected t))
                                   (get-not-selected-children t)))
     (make-tree(tree-node t)(remove-trees-from-t-fn (tree-children t)))))
;_________Tests
(begin-for-test(check-equal?(remove-tree-from-t-fn tree10-square)tree-10-sq-for-remove1)
               (check-equal?(remove-tree-from-t-fn tree10-deeper-selection)tree10-deeper-removal1))
;;__________________________________________________________________
;;remove-tree-from-t-fn : tree -> tree
;;GIVEN: Tree
;RETURNS: Tree after removing the selected tree node from the tree and adding its children to the
;parent
;;STRATEGY: Use template of trees on ts
;;HALTING-MEASURE:Length of list
(define (remove-trees-from-t-fn ts)
  (cond
    [(empty? ts) ts]
    [else (cons(remove-tree-from-t-fn (first ts))
               (remove-trees-from-t-fn(rest ts)))]))
;;__________________________________________________________________   
;;get-not-selected-children : tree -> list of tree
;;GIVEN: Tree
;RETURNS: list of not selected children of the tree
;;STRATEGY: Use template of trees on ts
;;HALTING-MEASURE:Length of list
(define (get-not-selected-children t)
  ;;given: tree
  ;;returns:return children if node not selected
  (filter(lambda(t1) (not(node-selected? (tree-node t1))))
         (tree-children t)))
;;___________Tests:
(begin-for-test(check-equal?(get-not-selected-children tree10-square)tree-10-sq-no-sel-children))
;;__________________________________________________________________
;;is-child-selected: Tree-> Children of tree
;;GIVEN:Tree
;;RETURN:Child of tree which is selected
;;STRATEGY:Use of HOF ormap on t
(define(is-child-selected? t)
  ;;given: tree
  ;;returns:returns children of the tree that are selected
  (ormap(lambda(t1) (node-selected? (tree-node t1))) (tree-children t)))
;;___________Tests:
(begin-for-test(check-equal?(is-child-selected? tree9)#t))

;;__________________________________________________________________
;;get-child-selected: Tree
;;GIVEN: Tree
;;RETURNS: selected tree from root Tree's Children
;;EXAMPLES:(get-child-selected tree9)=>(list(make-tree(make-node CIRCLE #t 120 300 0 0)empty))
;;STRATEGY:Use HOF Foldr on t

(define(get-child-selected t)
  ;;given:child of tree
  ;;returns:in list of root's children if node of any child is selected return its children 
(foldr(lambda(t1 sel) (if(node-selected? (tree-node t1)) (cons t1 sel) sel)) empty (tree-children t)))
;;___________Tests:
(begin-for-test(check-equal?(get-child-selected tree9)
                            (list(make-tree(make-node CIRCLE #t 120 300 0 0)empty))))
(begin-for-test(check-equal?(get-child-selected tree9)
                            (list(make-tree(make-node CIRCLE #t 120 300 0 0)empty))))

;;__________________________________________________________________
;;get-children-of-selected-node: list of t-> list of trees
;;GIVEN: takes list of selected trees
;;RETURNS: returns the children of the selected tree in (tree-children)
;;EXAMPLES:(get-child-of-selected-node tree8)=>(list(make-tree(make-node CIRCLE #f 17 43  0 0)'()))
;;STRATEGY:Use HOF Foldr on tree-children
(define(get-children-of-selected-node list-of-selected-trees)

  ;;given:tree
  ;;returns: checks if node selected then returns a list of its children
  (foldr(lambda (t1 children)
        (if(node-selected? (tree-node t1))
           (append(tree-children t1) children)
           children))
     empty list-of-selected-trees))

;__________Tests:
(begin-for-test(check-equal?(get-children-of-selected-node (list tree81))tree82)
               (check-equal?(get-children-of-selected-node (list tree-unselected))
                            empty))
;;__________________________________________________________________
;;check-tree-selected?: w-> Boolean
;;GIVEN: World i.e List of tree
;;RETURN:True iff any tree is selected by the mouse event
;;STRATEGY: Use of HOF Ormap on world-lot
;;EXAMPLES:(check-world-selected?(make-world (list tree10)))=>#t
(define (check-world-selected? w)
  ;;given:tree
  ;;returns:tree if the world has a tree selected
  (ormap(lambda(t1)(check-tree-selected? t1)) (world-lot w)))

(begin-for-test(check-equal?(check-world-selected?(make-world (list tree10)))#t))
(begin-for-test(check-equal?(check-world-selected?(make-world (list tree-unselected)))#f))
;;__________________________________________________________________
;;check-tree-selected?: t-> boolean
;;GIVEN: Tree
;;RETURNS: True iff any node of the tree is selected
;;EXAMPLES:(check-tree-selected? tree10)=> #t
;;(check-tree-selected? tree-unselected)=>#f
;;SRATEGY: Use template for Tree on t
(define(check-tree-selected? t)
  (if(node-selected? (tree-node t))
     #t
     (check-trees-selected? (tree-children t))))
;---Tests 
(begin-for-test(check-equal? (check-tree-selected? tree10)#t))
(begin-for-test(check-equal? (check-tree-selected? tree-unselected)#f))
;;__________________________________________________________________
;;check-trees-selected?: ts-> boolean
;;GIVEN: Trees
;;RETURNS: True iff any node of the trees is selected
;;EXAMPLES:(check-trees-selected? tree10)=> #t
;;(check-trees-selected? tree-unselected)=>#f
;;SRATEGY: Use HOF Ormap on ts
(define(check-trees-selected? ts)
  ;;given: tree
  ;;returns false if no tree in children selected
(ormap(lambda(t1)(check-tree-selected? t1)) ts))
;;__________________________________________________________________
;;creating-new-tree-fn: tree-> tree
;;GIVEN: Tree
;;RETURNS: Tree with a new list of trees if the node is selected
;;EXAMPLES: (check-equal?(creating-new-tree-fn tree1)=>tree2
;;STRATEGY: Use template for Tree on t
(define (creating-new-tree-fn t shape)
  (cond
    [(check-eligible-new? t)
     (make-new-tree t shape)]
    [(check-eligible-old? t)
     (make-old-tree t shape)]
    [else (make-tree(tree-node t)(creating-new-trees-fn (tree-children t) shape))]))
;-----Tests
(begin-for-test(check-equal?(creating-new-tree-fn tree1 CIRCLE)tree2)
               (check-equal?(creating-new-tree-fn tree3 CIRCLE)tree4)
               (check-equal?(creating-new-tree-fn tree7 CIRCLE)tree8)
               (check-equal?(creating-new-tree-fn tree9 CIRCLE)tree10)
               (check-equal?(creating-new-tree-fn tree9 SQUARE)tree10-square))
;;__________________________________________________________________
;;creating-new-trees-fn: List Of trees(children)
;;GIVEN:Trees
;;RETURNS: Trees i.e list of all their descendants
;EXAMPLES:(creating-new-tree-fn tree1 CIRCLE)=>tree2
;;STRATEGY: Use template for Trees on ts
;;HALTING-MEASURE: Length of list ts
(define (creating-new-trees-fn ts shape)
  (cond
    [(empty? ts) empty]
    [else (cons(creating-new-tree-fn (first ts) shape)
               (creating-new-trees-fn (rest ts) shape))]))
;;__________________________________________________________________
;;check-eligible-new?: tree-> Boolean
;;GIVEN:" tree
;RETURNS: true iff node is selected and tree has no children
;;EXAMPLES:(make-tree(make-node "circle" #t 40 38 0 0)'())=> #t
;;STRATEGY:;;Use template for Tree on t
(define (check-eligible-new? t)
  (and(node-selected?(tree-node t)) (equal? (tree-children t) empty)))
;-----Tests
(begin-for-test(check-equal?(check-eligible-new?(make-tree(make-node "circle" #t 40 38 0 0)'()))#t))
;;__________________________________________________________________
;;node-to-selected? : Node -> Boolean
;;GIVEN: Node
;;RETURNS: true iff the given node is selected
;;STRATEGY:Use template for Node on n
(define (node-to-selected? n)
  (node-selected? n))
;-----Tests
(begin-for-test(check-equal? (node-to-selected?(make-node "circle" #t 40 38 0 0)) #t))
;;__________________________________________________________________
;;check-eligibility-old? : tree-> Boolean
;;GIVEN: Tree
;;RETURNS: True iff the node is selected and has children
;;EXAMPLE:(check-eligible-old? tree2)=> #t
;;STRATEGY: Use template for Tree on t
(define (check-eligible-old? t)
  (and(node-selected?(tree-node t)) (not(equal? (tree-children t) empty))))
;-----Tests
(begin-for-test(check-equal?(check-eligible-old? tree2)#t))
;;__________________________________________________________________
;;make-new-tree: tree-> tree
;;GIVEN: Tree
;;RETURNS: Tree with a new tree added to its children
;;EXAMPLES:(make-new-tree (make-tree(make-node "circle" #t 40 37 0 0) empty))=>
;;(make-tree(make-node "circle" #t 40 37 0 0)(list(make-tree(make-node CIRCLE #f 40 40 0 0) empty)))
;;STRATEGY: Combine Simpler Functions
(define (make-new-tree t shape)
  (make-tree (tree-node t)(cons(make-tree(make-node shape
                                                    #f
                                                    (node-x (tree-node t))
                                                    (+(node-y (tree-node t)) (* 3 CIRCLE-RADIUS) )
                                                    (node-dx (tree-node t))
                                                    (node-dy (tree-node t)))
                                         empty)empty)))
;-----Tests
(begin-for-test(check-equal?(make-new-tree
                             (make-tree(make-node CIRCLE #t 400 370 0 0)empty)CIRCLE)
                            (make-tree(make-node CIRCLE #t 400 370 0 0)
                                      (list(make-tree(make-node CIRCLE #f 400 430 0 0) empty))))
               (check-equal?(make-new-tree
                             (make-tree(make-node CIRCLE #t 400 370 0 0)empty)SQUARE)
                            (make-tree(make-node CIRCLE #t 400 370 0 0)
                                      (list(make-tree(make-node SQUARE #f 400 430 0 0) empty)))))
;;;__________________________________________________________________
;;newx-node: children(list of trees)-> NonNegINt
;;GIVEN: Children(List of Trees)
;;RETURNS:NonNegInt
;;EXAMPLES:(newx-node (list(make-tree(make-node "circle" #f 40 40  0 0)empty)
;                                             (make-tree(make-node "circle" #f 30 40 0 0)empty)
;                                             (make-tree(make-node "circle" #f 20 40 0 0)empty)))=17
;;STRATEGY: combine simpler functions
(define (newx-node ts)
  ( - (min-x-value ts) (* 3 CIRCLE-RADIUS)))
;-----Tests
(begin-for-test(check-equal? (newx-node (list(make-tree(make-node "circle" #f 400 400 0 0)empty)
                                             (make-tree(make-node "circle" #f 300 400  0 0)empty)
                                             (make-tree(make-node "circle" #f 200 400 0 0)empty)))
                             140))
;;__________________________________________________________________
;;min-x-value: trees-> NonNegInt 
;;GUVEN: List of trees (children if tree)
;;RETURNS: Integer for the lowest x-coordinate in the list
;;EXAMPLES: (min-x-value (list(make-tree(make-node "circle" #f 40 40  )empty)
;;                                              (make-tree(make-node "circle" #f 30 40 0 0)empty)
;;                                              (make-tree(make-node "circle" #f 20 40 0 0)empty)))
;;=>20
;;STRATEGY: Use HOF Foldr on ts
(define (min-x-value ts)
  ;;given: tree
  ;;returns: list of x values of the nodes in the list of tree
  (first(sort(foldr(lambda (t1 lx) (cons(node-x (tree-node t1))lx)) empty ts) <)))

;-----Tests
(begin-for-test(check-equal?(min-x-value (list(make-tree(make-node "circle" #f 40 40 0 0)empty)
                                              (make-tree(make-node "circle" #f 30 40 0 0)empty)
                                              (make-tree(make-node "circle" #f 20 40 0 0)empty)))20))
;;__________________________________________________________________
;;make-old-tree: tree->tree
;;GIVEN: Tree
;;RETURN: Tree with a new tree added to its children
;;STRATEGY:Combine Simpler Functions
;;EXAMPLES:(make-old-tree tree5)=>tree6
(define (make-old-tree t shape)
  (make-tree (tree-node t)(cons(make-tree(make-node shape #f (newx-node (tree-children t))
                                                    (+ (node-y (tree-node t)) (* 3 CIRCLE-RADIUS))
                                                    (node-dx (tree-node t))
                                                    (node-dy (tree-node t)))
                                         empty)
                               (tree-children t))))
;-----Tests
(begin-for-test(check-equal?(make-old-tree tree5 CIRCLE)tree6)
               (check-equal?(make-old-tree tree5 SQUARE)tree6-square))
;;__________________________________________________________________
;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: use template for Worldstate on w
(define (world-after-mouse-event w mx my mev)
  (make-world
   ;;given:tree
   ;;return: list of tree after the mouse event
   (foldr(lambda(w1 lt) (cons(tree-after-mouse-event w1 mx my mev) lt)) empty (world-lot w))
    ))
;-----Tests
(begin-for-test(check-equal?(world-after-mouse-event (make-world(list tree5 tree1)) 40 37 "move")
                            (make-world(list tree5 tree1))))


;;__________________________________________________________________
;; tree-after-mouse-event : circl Integer Integer MouseEvent -> circl
;; GIVEN: a circl and a description of a mouse event
;; RETURNS: the circl that should follow the given mouse event
;;EXAMPLES:(tree-after-mouse-event tree5 40 37 "move")=>tree5
;; STRATEGY: use template for MousEevent mev
(define (tree-after-mouse-event t mx my mev)
  (cond
    [(mouse=? mev "button-down") (tree-after-button-down t mx my)]
    [(mouse=? mev "drag") (drag-tree-top t mx my)]
    [(mouse=? mev "button-up") (tree-after-button-up t mx my)]
    [else t]))
;-----Tests
(begin-for-test(check-equal?(tree-after-mouse-event (make-tree(make-node CIRCLE #f 40 37 0 0)
                                      (list(make-tree(make-node CIRCLE #f 10 40 0 0 )empty)
                                           (make-tree(make-node CIRCLE #f 19 40 0 0 )empty)))
                                                    40 37 "button-down")
                            (make-tree(make-node CIRCLE #t 40 37 0 0)
                                      (list(make-tree(make-node CIRCLE #f 10 40 0 0 )empty)
                                           (make-tree(make-node CIRCLE #f 19 40 0 0 )empty)))))
(begin-for-test(check-equal?(tree-after-mouse-event tree5 40 37 "move")tree5))
(begin-for-test(check-equal?(tree-after-mouse-event tree2-mouseb 25 25 "drag")
                            (make-tree(make-node "circle" #t 35 35 10 10)
                        (list(make-tree (make-node "circle" #f 15 38 0 0) empty)
                        (make-tree (make-node "circle" #f 5 38 0 0) empty)))))
(begin-for-test(check-equal?(tree-after-mouse-event tree-unselected 0 0 "button-up")tree-unselected))
;;;__________________________________________________________________
;; drag-trees-top : ListOfTree NonNegInt NonNegInt -> ListOfTree
;; GIVEN: A listoftree and two nonegints
;; RETURNS: A listoftree after a drag event
;;STRATEGY: Combine Simpler functions
;;HALTING-MEASURE: Length of list lot
(define (drag-trees-top lot mx my)
  (cond
   [(empty? lot) lot]
   [else (cons (drag-tree-top (first lot) mx my) (drag-trees-top (rest lot) mx my))]
   )
  )
;;;__________________________________________________________________
;; drag-tree-top : Tree NonNegInt NonNegInt -> Tree
;; GIVEN: A tree and Mouse x and Mouse y pointers
;; RETURNS: A tree that has been dragged the appropriate distance in response to
;;          a smooth drag
;;STRATEGY: Template for Tree on t
(define (drag-tree-top t mx my)
  (if (node-selected? (tree-node t))
      (make-tree (make-node (node-shape (tree-node t))
                        (node-selected? (tree-node t))
                        (+ (node-dx (tree-node t)) mx)
                        (+ (node-dy (tree-node t)) my)
                        (node-dx (tree-node t))
                        (node-dy (tree-node t)) 
                        )
             
             (drag-trees (tree-children t)
                        (- (+ (node-dx (tree-node t)) mx) (node-x (tree-node t)))
                        (- (+ (node-dy (tree-node t)) my) (node-y (tree-node t)))))
      
      (if (not (empty? (tree-children t))) (make-tree (tree-node t) (drag-trees-top (tree-children t)
                                                                           mx my)) t)))
;-----Tests
(define an_example_tree(make-tree(make-node CIRCLE #f 40 37 0 0)
                                 (list(make-tree(make-node CIRCLE #t 17 40 0 0  )empty)
                                      (make-tree(make-node CIRCLE #f 40 40 0 0  )empty)
                                      (make-tree(make-node CIRCLE #f 30 40 0 0  )empty)
                                      (make-tree(make-node CIRCLE #f 20 40 0 0  )empty))))

(begin-for-test (check-equal? (drag-tree-top an_example_tree 20 20)
 (make-tree
 (make-node "circle" #false 40 37 0 0)
 (list
  (make-tree (make-node "circle" #true 20 20 0 0) '())
  (make-tree (make-node "circle" #false 40 40 0 0) '())
  (make-tree (make-node "circle" #false 30 40 0 0) '())
  (make-tree (make-node "circle" #false 20 40 0 0) '())))))
(begin-for-test (check-equal? (drag-tree-top tree2-mouseb 25 25)
                             (make-tree(make-node "circle" #t 35 35 10 10)
                        (list(make-tree (make-node "circle" #f 15 38 0 0) empty)
                        (make-tree (make-node "circle" #f 5 38 0 0) empty))))
                (check-equal? (drag-tree-top tree6b 60 25)
                              (make-tree(make-node CIRCLE #t 55 20  -5 -5)
                                        (list(make-tree(make-node CIRCLE #f 32 23 0 0)empty)
                                             (make-tree(make-node CIRCLE #f 55 23 0 0)empty)
                                             (make-tree(make-node CIRCLE #f 45 23 0 0)empty)
                                             (make-tree(make-node CIRCLE #f 35 23 0 0)empty))))) 

;;;__________________________________________________________________
;; drag-tree : Tree Integer Integer -> Tree
;; GIVEN: A tree and two integers
;; RETURNS: A tree whose nodes have been dragged the appropriate distance
;;          in response to a smooth drag
;;STRATEGY: Template for Tree on t
(define (drag-tree t x_move y_move)
  (make-tree (make-node (node-shape (tree-node t))
                        (node-selected? (tree-node t))
                        (+ (node-x (tree-node t)) x_move)
                        (+ (node-y (tree-node t)) y_move)
                        (node-dx (tree-node t))
                        (node-dy (tree-node t)))
             (drag-trees (tree-children t) x_move y_move)))

;;;__________________________________________________________________
;; drag-trees : ListOfTree Integer Integer -> ListOfTree
;; GIVEN: A listoftree and two integers
;; RETURNS: A ListofTree whose nodes have been dragged the appropriate distance
;;         in response to a smooth drag
;;STRATEGY: Template for Trees on ts
;;HALTING-MEASURE: Length of list ts
(define (drag-trees ts x_move y_move)
  (cond
    [(empty? ts) empty]
    [else (cons (drag-tree (first ts) x_move y_move) (drag-trees (rest ts) x_move y_move))]
    )
  )
(begin-for-test (check-equal? (drag-trees-top test_lot 10 10) test_lot2))

  (define test_lot (list(make-tree(make-node CIRCLE #t 32 23 5 5)empty)
    (make-tree(make-node CIRCLE #f 55 23 0 0)empty)
    (make-tree(make-node CIRCLE #f 45 23 0 0)empty)
    (make-tree(make-node CIRCLE #f 35 23 0 0)empty)))
  (define test_lot2 (list(make-tree(make-node CIRCLE #t 15 15 5 5)empty) 
    (make-tree(make-node CIRCLE #f 55 23 0 0)empty)
    (make-tree(make-node CIRCLE #f 45 23 0 0)empty)
    (make-tree(make-node CIRCLE #f 35 23 0 0)empty)))
;;__________________________________________________________________    
;;tree-after-button-up: tree mx my
;;GIVEN: Tree Mouse x Mouse y
;;RETURNS:tree with node-selected? false as no node should be selected if button up
;;STRATEGY:Use of Template for Tree on t
(define(tree-after-button-up t mx my)
  (make-tree(make-node (node-shape (tree-node t))
                       #f
                       (node-x (tree-node t))
                       (node-y (tree-node t))
                       (node-dx (tree-node t))
                       (node-dy (tree-node t)))
            (trees-after-button-up(tree-children t) mx my)))
;;__________________________________________________________________
;;trees-after-button-up: trees mx my
;;GIVEN: Trees Mouse x Mouse y
;;RETURNS:trees with node-selected? false as no node should be selected if button up
;;STRATEGY:Use of Template for Trees on ts
;;HALTING-MEASURE: Length of list lot
(define(trees-after-button-up ts mx my)
  (cond[(empty? ts) empty]
       [else(cons(tree-after-button-up(first ts) mx my)
                 (trees-after-button-up(rest ts) mx my))]))

(begin-for-test(check-equal?(tree-after-button-up tree3-mouse 40 40)tree1-mouse))

;;__________________________________________________________________
;;tree-after-button-down tree mx my
;;GIVEN: Tree Mouse x Mouse y
;;RETURNS:tree with node-selected? true iff mouse-down event occurs inside the circle
;;as one node should be selected if button down on it
;;EXAMPLES:(tree-after-button-down tree1-mouse 40 37)=> tree2-mouse
;;STRATEGY:Use of Template for Tree on t
(define(tree-after-button-down t mx my)
  (make-tree(node-after-mouse-button-down (tree-node t) mx my (node-shape (tree-node t)))
            (trees-after-button-down(tree-children t) mx my)))
(begin-for-test
  (check-equal?
   (tree-after-button-down (make-tree(make-node CIRCLE #f 40 37 0 0)
                                     (list(make-tree(make-node CIRCLE #f 42 35 0 0 )empty)))40 37)
                            (make-tree(make-node CIRCLE #t 40 37 0 0)
                                      (list(make-tree(make-node CIRCLE #t 42 35 2 -2  )empty)))))
;;__________________________________________________________________
;;trees-after-button-down tree mx my
;;GIVEN: Tree Mouse x Mouse y
;;RETURNS:tree with node-selected? true iff mouse-down event occurs inside the circle
;;as one node should be selected if button down on it
;;EXAMPLES:(tree-after-button-down tree1-mouse 40 37)=> tree2-mouse
;;STRATEGY:Use of Template for Trees on t
;;HALTING-MEASURE: Length of list ts
 (define(trees-after-button-down ts mx my)
   (cond[(empty? ts) empty]
        [else (cons(tree-after-button-down(first ts) mx my)
                   (trees-after-button-down (rest ts) mx my))]))
 (begin-for-test(check-equal?(tree-after-button-down tree1-mouse 40 37) tree2-mouse))
;;__________________________________________________________________
;; node-after-mouse-button-down:  Integer Integer shape -> Boolean
;; GIVEN: Node Integer Integer
;; RETURNS: true iff the given coordinate is inside the bounding box of
;; the given shape.
;; EXAMPLES: 
;; STRATEGY:Use template for Node on n
(define (node-after-mouse-button-down n mx my shape)
  (cond
    [(and (equal? shape CIRCLE)(inside-circle? n mx my))
     (make-node (node-shape n) #t (node-x n)(node-y n)(-(node-x n) mx) (-(node-y n) my))]
    [(and (equal? shape SQUARE)(inside-square? n mx my))
     (make-node (node-shape n) #t (node-x n)(node-y n)(-(node-x n) mx) (-(node-y n) my))]
    [else n]))
(begin-for-test(check-equal?(node-after-mouse-button-down(make-node CIRCLE #f 40 30 0 0) 10 10 CIRCLE)
                            (make-node CIRCLE #f 40 30 0 0)))

(begin-for-test(check-equal?(node-after-mouse-button-down(make-node CIRCLE #f 40 30 0 0) 40 30 CIRCLE)
                            (make-node CIRCLE #t 40 30 0 0)))
(begin-for-test(check-equal?(node-after-mouse-button-down(make-node SQUARE #f 40 30 0 0) 20 30 SQUARE)
                            (make-node SQUARE #t 40 30 20 0)))

;;__________________________________________________________________
;;inside-circle?: n mx my-> Boolean
;;GIVEN: Node mouse-x Mouse-y
;;RETURNS: True iff the mouse point is inside the circle boundary
;;EXAMPLE:(inside-circle? (make-node CIRCLE #f 40 30 0 0) 10 10)=> #f
;;(inside-circle? (make-node CIRCLE #f 40 30 0 0) 40 30)=> #t
;;STRATEGY:Use of Template of Node on n 

(define (inside-circle? n mx my)
    (<= (+ (sqr(- mx (node-x n)))
      (sqr(- my (node-y n)))) (sqr CIRCLE-RADIUS)))
;_____Tests
  (begin-for-test(check-equal?(inside-circle? (make-node CIRCLE #f 40 30 0 0) 10 10)#f))
  (begin-for-test(check-equal?(inside-circle? (make-node CIRCLE #f 40 30 0 0) 40 30)#t))
;;__________________________________________________________________  
;;inside-square?: n mx my-> Boolean
;;GIVEN: Node mouse point x mouse point y
;;RETURNS: True iff the mouse point is inside the square boundary
;;EXAMPLE:(inside-square? (make-node CIRCLE #f 40 30 0 0) 10 10)=> #f
;;(inside-square? (make-node CIRCLE #f 40 30 0 0) 40 30)=> #t
;;STRATEGY:Use of Template of Node on n 
  (define (inside-square? n mx my)
    (and(and(>= mx (- (node-x n)  CIRCLE-RADIUS))(<= mx (+ (node-x n)  CIRCLE-RADIUS)))
        (and(>= my (- (node-y n)  CIRCLE-RADIUS))(<= my (+ (node-y n)  CIRCLE-RADIUS)))))
      
  (begin-for-test(check-equal?(inside-square? (make-node SQUARE #f 40 30 0 0) 19 30)#f))
  (begin-for-test(check-equal?(inside-square? (make-node SQUARE #f 40 30 0 0) 20 30)#t))
;;##########################################################################################
;;##########################################################################################
;; place-shape : Node Scene -> Scene
;; GIVEN: A node and a scene
;; RETURNS: A scene with the node drawn onto the scene
;; DESIGN STRATEGY: Use template for node on n
(define (place-shape n s)
  (cond
    [(and (equal? (node-shape n) SQUARE) (node-selected? n))
     (place-image (square (* CIRCLE-RADIUS 2) "solid" "green")
                  (node-x n) (node-y n) s)]
    [(and (equal? (node-shape n) SQUARE) (not (node-selected? n)))
     (place-image (square (* CIRCLE-RADIUS 2) "outline" "green")(node-x n) (node-y n) s)]
    [(and (equal? (node-shape n) CIRCLE) (node-selected? n))
     (place-image (circle CIRCLE-RADIUS "solid" "green") (node-x n) (node-y n) s)]
    [(and (equal? (node-shape n) CIRCLE) (not (node-selected? n)))
     (place-image (circle CIRCLE-RADIUS "outline" "green")(node-x n) (node-y n) s)]
    )
  )
(begin-for-test(check-equal?(place-shape (make-node SQUARE #f 40 30 0 0) EMPTY-CANVAS)
                            (place-image (square (* CIRCLE-RADIUS 2) "outline" "green")
                  40 30 EMPTY-CANVAS))
               (check-equal?(place-shape (make-node SQUARE #t 40 30 0 0) EMPTY-CANVAS)
                            (place-image (square (* CIRCLE-RADIUS 2) "solid" "green")
                  40 30 EMPTY-CANVAS))
               (check-equal?(place-shape (make-node CIRCLE #t 40 30 0 0) EMPTY-CANVAS)
                            (place-image (circle CIRCLE-RADIUS "solid" "green")
                  40 30 EMPTY-CANVAS)))

;;__________________________________________________________________  
;; draw-tree-on-scene : Tree Scene -> Scene
;; GIVEN: A tree and a scene
;; RETURNS: A Scene with the nodes of the tree rendered onto it
;; DESIGN STRATEGY: Use template for tree on t
(define (draw-tree-on-scene t s)
    (place-shape (tree-node t) (draw-trees-on-scene (tree-children t) s))
  )
;;__________________________________________________________________  
;; draw-trees-on-scene : ListOfTree Scene -> Scene
;; GIVEN: A listoftree and a scene
;; RETURNS: A Scene with the nodes of the trees rendered onto it
;; DESIGN STRATEGY: Use template for trees on ts
;;HALTING-MEASURE: Length of list ts
(define (draw-trees-on-scene ts s)
  (cond
    [(empty? ts) s]
    [else (draw-tree-on-scene (first ts) (draw-trees-on-scene (rest ts) s))]
    )
  )

(define (draw-world w)
  (cond
    [(empty? (world-lot w)) EMPTY-CANVAS]
    [else (draw-trees-line-on-scene (world-lot w)
                                    (draw-tree-on-scene (first (world-lot w))
                                                        (draw-trees-on-scene (rest(world-lot w))
                                                                             EMPTY-CANVAS)))]))
(begin-for-test(check-equal? (draw-world (make-world empty))EMPTY-CANVAS))

;;------------------------------------------------------------------
;; draw-tree-line-on-scene : Tree Scene -> Scene
;; GIVEN: A tree and a scene
;; RETURNS: A Scene with the lines of the tree rendered onto it
;; DESIGN STRATEGY: Use template for tree on t
;;HALTING-MEASURE: Length of list tree-children
(define (draw-tree-line-on-scene t s)
  (cond
    [(empty? (tree-children t)) s]
    [else (scene+line
           (draw-tree-line-on-scene (make-tree (tree-node t) (rest (tree-children t)))
                                    (draw-trees-line-on-scene (tree-children t) s)) 
           (node-x (tree-node t))
                (node-y (tree-node t))
                (node-x (tree-node (first (tree-children t))))
                (node-y (tree-node (first (tree-children t)))) "blue")] 
    )
  
  )

;; draw-trees-line-on-scene : ListOfTree Scene -> Scene
;; GIVEN: A listoftree and a scene
;; RETURNS: A Scene with the lines of the trees rendered onto it
;; DESIGN STRATEGY: Use template for trees on ts
;;HALTING-MEASURE: Length of list ts
(define (draw-trees-line-on-scene ts s)
  (cond
    [(empty? ts) s] 
    [else (draw-tree-line-on-scene (first ts) (draw-trees-line-on-scene (rest ts) s))]
    )
  )

;;__________________________________________________________________
;;world-to-scene : World -> Scene
;;GIVEN: World
;;RETURNS: Scene
;;EXAMPLES:
;;STRATEGY:Use Template for World on w

(define (world-to-scene w)
  (draw-world w))
(begin-for-test(check-equal?(world-to-scene (make-world world-removal2))
                            (draw-world (make-world world-removal2))))
;;__________________________________________________________________  
;;##########################################################################################
;;##########################################################################################
;;##########################################################################################
(define tree1-mouse (make-tree(make-node "circle" #f 40 37 0 0)
                        (list(make-tree (make-node "circle" #f 20 40 0 0) empty)
                             (make-tree (make-node "circle" #f 10 40 0 0) empty)
                             )))

(define tree2-mouse (make-tree(make-node "circle" #t 40 37 0 0)
                        (list(make-tree (make-node "circle" #f 20 40 0 0) empty)
                             (make-tree (make-node "circle" #f 10 40 0 0) empty)

                             )))



(define tree3-mouse (make-tree(make-node "circle" #t 40 37 0 0)
                        (list(make-tree (make-node "circle" #f 20 40 0 0) empty)
                             (make-tree (make-node "circle" #f 10 40 0 0) empty)
                             )))


(define tree1 (make-tree(make-node "circle" #t 400 370 0 0)
                        (list(make-tree (make-node "circle" #f 200 400 0 0) empty)
                             (make-tree (make-node "circle" #f 300 400 0 0) empty)
                             )))

(define tree2 (make-tree(make-node "circle" #t 400 370 0 0)
                        (list(make-tree (make-node "circle" #f 140 430 0 0) empty)
                             (make-tree (make-node "circle" #f 200 400 0 0) empty)
                             (make-tree (make-node "circle" #f 300 400 0 0) empty)
                             )))
(define tree3 (make-tree(make-node "circle" #t 400 370 0 0)'()))

(define tree4 (make-tree(make-node "circle" #t 400 370 0 0)
                        (list(make-tree(make-node "circle" #f 400 430 0 0)'()))))

(define tree5(make-tree(make-node CIRCLE #t 300 370 0 0)
                       (list(make-tree(make-node CIRCLE #f 140 300 0 0 )empty)
                            (make-tree(make-node CIRCLE #f 130 300 0 0 )empty)
                            (make-tree(make-node CIRCLE #f 120 300 0 0 )empty))))

(define tree6-square(make-tree(make-node CIRCLE #t 300 370 0 0)
                       (list(make-tree(make-node SQUARE #f 60 430 0 0 )empty)
                            (make-tree(make-node CIRCLE #f 140 300 0 0 )empty)
                            (make-tree(make-node CIRCLE #f 130 300 0 0 )empty)
                            (make-tree(make-node CIRCLE #f 120 300 0 0 )empty))))

(define tree6(make-tree(make-node CIRCLE #t 300 370 0 0)
                       (list(make-tree(make-node CIRCLE #f 60 430 0 0 )empty)
                        (make-tree(make-node CIRCLE #f 140 300 0 0 )empty)
                            (make-tree(make-node CIRCLE #f 130 300 0 0 )empty)
                            (make-tree(make-node CIRCLE #f 120 300 0 0 )empty))))

(define tree7(make-tree(make-node CIRCLE #f 400 370 0 0)
                       (list(make-tree(make-node CIRCLE #t 170 430 0 0  )empty))))

(define tree8(make-tree(make-node CIRCLE #f 400 370 0 0)
                       (list(make-tree(make-node CIRCLE #t 170 430 0 0  )
                                      (list(make-tree(make-node CIRCLE #f 170 490 0 0)'()))))))
(define tree81(make-tree(make-node CIRCLE #t 40 37 0 0)
                        (list(make-tree(make-node CIRCLE #f 17 40 0 0  )
                                       (list(make-tree(make-node CIRCLE #f 17 43 0 0)'()))))))
(define tree82(list(make-tree(make-node CIRCLE #f 17 40 0 0  )
                             (list(make-tree(make-node CIRCLE #f 17 43 0 0)'())))))

(define tree9(make-tree(make-node CIRCLE #f 200 300 0 0)
                       (list(make-tree(make-node CIRCLE #f 170 300 0 0)empty)
                            (make-tree(make-node CIRCLE #f 140 300 0 0)empty)
                            (make-tree(make-node CIRCLE #f 130 300 0 0)empty)
                            (make-tree(make-node CIRCLE #t 120 300 0 0)empty))))
(define tree9-remo(make-tree(make-node CIRCLE #f 200 300 0 0)
                       (list(make-tree(make-node CIRCLE #f 170 300 0 0)empty)
                            (make-tree(make-node CIRCLE #t 140 300 0 0)empty)
                            (make-tree(make-node CIRCLE #f 130 300 0 0)empty)
                            (make-tree(make-node CIRCLE #t 120 300 0 0)empty))))
(define tree9-remo1(make-tree(make-node CIRCLE #f 200 300 0 0)
                       (list(make-tree(make-node CIRCLE #f 170 300 0 0)empty)
                            (make-tree(make-node CIRCLE #t 140 300 0 0)(list
(make-tree(make-node CIRCLE #f 130 300 0 0)empty)(make-tree(make-node CIRCLE #f 130 300 0 0)empty)))
                            (make-tree(make-node CIRCLE #f 130 300 0 0)empty)
(make-tree(make-node CIRCLE #t 120 300 0 0)(list(make-tree(make-node CIRCLE #f 130 300 0 0)empty))))))

(define tree10(make-tree(make-node CIRCLE #f 200 300 0 0)
                        (list(make-tree(make-node CIRCLE #f 170 300 0 0)empty)
                             (make-tree(make-node CIRCLE #f 140 300 0 0)empty)
                             (make-tree(make-node CIRCLE #f 130 300 0 0)empty)
                             (make-tree(make-node CIRCLE #t 120 300 0 0)
                                       (list(make-tree(make-node CIRCLE #f 120 360 0 0)'()))))))

(define tree10-square(make-tree(make-node CIRCLE #f 200 300 0 0)
                        (list(make-tree(make-node CIRCLE #f 170 300 0 0)empty)
                             (make-tree(make-node CIRCLE #f 140 300 0 0)empty)
                             (make-tree(make-node CIRCLE #f 130 300 0 0)empty)
                             (make-tree(make-node CIRCLE #t 120 300 0 0)
                                       (list(make-tree(make-node SQUARE #f 120 360 0 0)'()))))))
(define tree10-deeper-selection
  (make-tree(make-node CIRCLE #f 40 37 0 0)
            (list(make-tree(make-node CIRCLE #f 17 40 0 0)empty)
                 (make-tree(make-node CIRCLE #f 40 40 0 0)empty)
                 (make-tree(make-node CIRCLE #f 30 40 0 0)empty)
                 (make-tree(make-node CIRCLE #f 20 40 0 0)
                           (list(make-tree(make-node SQUARE #t 20 43 0 0)
                                          (list(make-tree(make-node SQUARE #f 18 40 0 0)'()))
                                          ))))))
(define tree10-deeper-removal1
  (make-tree(make-node CIRCLE #f 40 37 0 0)
            (list(make-tree(make-node CIRCLE #f 17 40 0 0 )empty)
                 (make-tree(make-node CIRCLE #f 40 40 0 0  )empty)
                 (make-tree(make-node CIRCLE #f 30 40 0 0  )empty)
                 (make-tree(make-node CIRCLE #f 20 40 0 0  )
                           (list(make-tree(make-node SQUARE #f 18 40 0 0)'()))))))
(define tree-10-sq-no-sel-children
  (list(make-tree(make-node CIRCLE #f 170 300 0 0)empty)
       (make-tree(make-node CIRCLE #f 140 300 0 0)empty)
       (make-tree(make-node CIRCLE #f 130 300 0 0)empty)))

(define tree-10-sq-with-child-sel-node
  (list (make-tree(make-node SQUARE #f 20 43 0 0  )empty)
        (make-tree(make-node CIRCLE #f 17 40 0 0  )empty)
        (make-tree(make-node CIRCLE #f 40 40 0 0  )empty)
        (make-tree(make-node CIRCLE #f 30 40 0 0  )empty)
        ))

(define tree-10-sq-for-remove1
  (make-tree(make-node CIRCLE #f 200 300 0 0)
            (list(make-tree(make-node SQUARE #f 120 360 0 0)'())
                           (make-tree(make-node CIRCLE #f 170 300 0 0)empty)
                             (make-tree(make-node CIRCLE #f 140 300 0 0)empty)
                             (make-tree(make-node CIRCLE #f 130 300 0 0)empty)
                             )))
(define tree-unselected(make-tree(make-node CIRCLE #f 40 37 0 0)
                                 (list(make-tree(make-node CIRCLE #f 17 40 0 0  )empty)
                                      (make-tree(make-node CIRCLE #f 40 40 0 0  )empty)
                                      (make-tree(make-node CIRCLE #f 30 40 0 0  )empty)
                                      (make-tree(make-node CIRCLE #f 20 40 0 0  )empty))))
(define world-root-removal1
  (list(make-tree(make-node "circle" #t 40 37 0 0)
                 (list(make-tree (make-node "circle" #f 20 40 0 0)
                                 (list(make-tree(make-node "circle" #f 21 40 0 0)
                                                empty)
                                      (make-tree (make-node "circle" #f 22 40 0 0)
                                                 empty)))
                      (make-tree (make-node "circle" #f 30 40 0 0)
                                 (list(make-tree (make-node "circle" #f 31 40 0 0)
                                                 empty)
                                      (make-tree (make-node "circle" #f 32 40 0 0)
                                                 empty)))
                      ))))
(define world-after-root-removal2
  (list(make-tree (make-node "circle" #f 20 40 0 0)
                  (list(make-tree (make-node "circle" #f 21 40 0 0)empty)
                       (make-tree (make-node "circle" #f 22 40 0 0) empty)))
       (make-tree (make-node "circle" #f 30 40 0 0)
                  (list(make-tree (make-node "circle" #f 31 40 0 0)empty)
                       (make-tree (make-node "circle" #f 32 40 0 0) empty)))
       ))

(define world-removal1
  (list(make-tree(make-node "circle" #f 40 37 0 0)
                 (list(make-tree (make-node "circle" #t 20 40 0 0)
                                 (list(make-tree (make-node "circle" #f 21 40 0 0)empty)
                                      (make-tree (make-node "circle" #f 22 40 0 0) empty)))
                      (make-tree (make-node "circle" #f 30 40 0 0)
                                 (list(make-tree (make-node "circle" #f 31 40 0 0)empty)
                                      (make-tree (make-node "circle" #f 32 40 0 0) empty)))
                      ))))
(define world-unselected1
  (list(make-tree(make-node "circle" #f 40 37 0 0)
                 (list(make-tree (make-node "circle" #f 20 40 0 0)
                                 (list(make-tree (make-node "circle" #f 21 40 0 0)empty)
                                      (make-tree (make-node "circle" #f 22 40 0 0) empty)))
                      (make-tree (make-node "circle" #f 30 40 0 0)
                                 (list(make-tree (make-node "circle" #f 31 40 0 0)empty)
                                      (make-tree (make-node "circle" #f 32 40 0 0) empty)))
                      ))))
(define world-removal2
  (list(make-tree(make-node "circle" #f 40 37 0 0)
                 (list(make-tree (make-node "circle" #f 21 40 0 0)empty)
                      (make-tree (make-node "circle" #f 22 40 0 0) empty)
                      (make-tree (make-node "circle" #f 30 40 0 0)
                                 (list(make-tree (make-node "circle" #f 31 40 0 0)empty)
                                      (make-tree (make-node "circle" #f 32 40 0 0) empty)))
                      ))))

(define tree2-mouseb (make-tree(make-node "circle" #t 40 37 10 10)
                        (list(make-tree (make-node "circle" #f 20 40 0 0) empty)
                             (make-tree (make-node "circle" #f 10 40 0 0) empty)

                         )))
(define tree6b(make-tree(make-node CIRCLE #t 40 37  -5 -5)
                       (list(make-tree(make-node CIRCLE #f 17 40 0 0)empty)
                            (make-tree(make-node CIRCLE #f 40 40 0 0)empty)
                           (make-tree(make-node CIRCLE #f 30 40 0 0)empty)
                          (make-tree(make-node CIRCLE #f 20 40 0 0)empty))))