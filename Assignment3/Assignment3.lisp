;Nodes are in the following format
;(state parent-state score depth)
;score is the score calculated by a heuristics function

;the open list used by the search algorithms
(defvar *open* nil)

;the closed list used by the search algorithms
(defvar *closed* nil)

;Function: make-children
;Description: this function generates the child nodes for a given parent node
;nodes are in the following format
;(state parent-state score depth)
;Parameters:
;parent-node: this is the parent node. we use it to derive the parent-state and depth
;goal-state, this is the goal state of the game
;moves: a function that takes a single state as a parameter
;and returns a list of child states
;heuristic: a function that takes a state as a parameter and returns a numerical 'score'
;Return: a list of child nodes for the given state
(defun make-children (parent-node goal-state moves heuristic)
    (let (
        (parent-state (first parent-node))
        (parent-score (third parent-node))
        (parent-depth (fourth parent-node))
        )
        (mapcar
            (lambda (x) (list x parent-state (+ parent-score (eval (list heuristic (list 'quote x) (list 'quote goal-state)))) (1+ parent-depth)))
             (remove-if
                 (lambda (x)
                     (or
                        (equal x parent-state)
                        (member x (mapcar #'car *open*) :test #'equal)
                        (member x (mapcar #'car *closed*) :test #'equal)
                    )
                )
                (remove nil (eval (list moves (list 'quote parent-state))))
            )
        )

    )
)

;Function: print-node
;Description: prints out a node
;Parameters: the node to print
;Return: nil
(defun print-node (node)
    (format t "~&Evaluating Node: ~a" node)
)

;Function: solution-path
;Description: returns a list containing the path from root state to goal state
;uses the *closed* list
;Parameters:
;node, the goal node
;Return: a list of states from root to node
(defun solution-path (node)
    (cond
        ((equal node nil)
            nil
        )
        ((equal (second node) nil)
            (list (car node))
        )
        (t
            (append
                (solution-path (elt *closed* (position (second node) (mapcar #'car *closed*))))
                (list (car node))
            )
        )
    )
)

;Function: print-solution
;Description: prints the length of *open* and *closed* and the solution path
;Parameters:
;path, a list of states from start to goal
;Return: the path
(defun print-solution (path)
    (cond
        ((format t "~&Length of *open*: ~a" (length *open*)))
        ((format t "~&Length of *closed*: ~a" (length *closed*)))
        ((format t "~&Solution Path Length: ~a" (length path)))
        ((format t "~&Solution Path: ~a" path))
        (t
            path
        )
    )

)

;Function: lmerge
;Description: merges two lists of nodes which must be sorted from low to high based on their score
;Parameters:
;l1: the first list being merged (must be sorted)
;l2: the second list being merged (must be sorted)
;Return: a merged list of nodes sorted from low to high based on their score
(defun lmerge (l1 l2)
    (cond
        ((null l1) l2)
        ((null l2) l1)
        (t
            (let
                (
                    (h1 (third (car l1)))
                    (h2 (third (car l2)))
                )
                (cond
                    ((< h1 h2)
                        (cons (car l1) (lmerge (cdr l1) l2))
                    )
                    ((< h2 h1)
                        (cons (car l2) (lmerge l1 (cdr l2)))
                    )
                    (t
                        (append
                            (list (car l1) (car l2))
                            (lmerge (cdr l1) (cdr l2))
                        )
                    )
                )
            )
        )
    )
)

;Function: merge-sort
;Description: sorts a list of nodes from low to high based on their score
;Parameters:
;l: the list of nodes to be sorted
;Return: the list of nodes sorted from low to high
(defun merge-sort (l)
    (cond
        ((<= (length l) 1)
            l
        )
        (t
            (lmerge
                (merge-sort (nthcdr (floor (/ (length l) 2)) l))
                (merge-sort (reverse (nthcdr (- (length l) (floor (/ (length l) 2))) (reverse l))))
            )
        )
    )
)

;Function: a*
;Description: uses the global *open* and *closed* lists
;to find a path to the goal from the start using the given heuristic function
;Parameters:
;heuristic, the name of a function that takes a state and a goal state
;and returns the estimated number of moves to reach the goal
;goal, the target state that we are trying to achieve
;iteration, the current number of nodes looked at
;moves, a function that takes a state as a parameter and  returns the child states
(defun a* (heuristic goal moves &optional max-depth)
    (let
        (
            (node (car *open*))
        )
        (cond
            ((>= 0 (length *open*))
                nil
            )
            ((print-node node))
            ((equal goal (first node)) node)
            (T
                (setf *closed* (cons node *closed*))
                (setf *open*
                    (lmerge
                        (cdr *open*)
                        (if (or (null max-depth) (< (fourth node) max-depth))
                            (merge-sort (make-children node goal moves heuristic))
                        )
                    )
                )
                (a* heuristic goal moves max-depth)
            )
        )
    )
)

;Function: heuristic-search
;Description: sets up the open and closed lists then calls
;the recursive function breadth-first-search to find a path to the goal state
;Parameters:
;heuristic, heuristic is the name of a function that takes a state and a goal state
;and returns the estimated number of moves to the goal state
;game, is a list with three elements where
;(first game), is the start state
;(second game), is the goal state
;(third game), is a function that takes a state and returns its valid children.
;(fourth game), is a heuristic function that takes a state and returns a numerical value
;Returns:
;the path from the start state to goal if it exists, otherwise nil
(defun heuristic-search (heuristic game &optional max-depth)
    (let (
        (start (first game))
        (goal (second game))
        (moves (third game))
        (score (eval (list heuristic (list 'quote (first game)) (list 'quote (second game)))))
    )
        (setf *open* (list (list start nil score 0)))
        (setf *closed* nil)
        (print-solution (solution-path (a* heuristic goal moves max-depth)))
    )
)

;Function: default-heuristic
;Description: takes a state and a goal-state and returns the number of items
;that are not the same in both states
;Parameters:
;state, the current game state
;goal-state, the goal state of the current game
;Returns:
;the number of items that are not in the correct position
(defun default-heuristic (state goal-state)
    (count nil
        (mapcar #'equal state goal-state)
    )
)

;*******************************************************************************************
;Farmer Wolf Goat Cabbage Puzzle
;*******************************************************************************************
;This is the state representation for the farmer, the wolf, the goat and the cabbage puzzle.
;the puzzle can be found at http://www.mathsisfun.com/puzzles/farmer-crosses-river.html
;a state is a list containing 4 elements (farmer wolf goat cabbage)
;all four items are either t or nil
;farmer, wolf, goat, and cabbage are t when they are on the far shore and nil when they are on the near shore
;start state
;(nil nil nil nil)
;goal state
;(t t t t)

;Function: fwgc-children
;Description: This functions generates child nodes for the farmer, wolf, goat, and cabbage puzzle
;it takes a state as a parameter and generates the legal child states
;Parameters:
;a state of the fwgc puzzle
;Returns: a list of legal child states
(defun fwgc-children (state)
    (let
        (
            (farmer (first state))
            (wolf (second state))
            (goat (third state))
            (cabbage (fourth state))
        )
        ;this will remove the illegal states
        (remove-if
            (lambda (x)
                (let
                    (
                        (farmer (first x))
                        (wolf (second x))
                        (goat (third x))
                        (cabbage (fourth x))
                    )
                    (and (not (equal goat farmer)) (or (equal goat wolf) (equal goat cabbage)))
                )
            )
            (remove nil
                (list
                    ;farmer does not taking anything
                    (list (not farmer) wolf goat cabbage)
                    ;farmer takes wolf if has wolf
                    (if (equal wolf farmer)
                        (list (not farmer) (not wolf) goat cabbage)
                    )
                    ;farmer takes goat if has goat
                    (if (equal goat farmer)
                        (list (not farmer) wolf (not goat) cabbage)
                    )
                    ;farmer takes cabbage if has cabbage
                    (if (equal cabbage farmer)
                        (list (not farmer) wolf goat (not cabbage))
                    )
                )
            )
        )
    )
)

;Function: fwgc
;Description: This functions returns a list which represents
;the farmer, wolf, goat, and the cabbage game
;where the first item is the start state,
;the second is the goal sate,
;and the third is a function that takes a state and generates the legal child states
;Parameters: none
;Returns: a list containing the game representation
(defun fwgc ()
    '( (t t t t) (t t t nil) fwgc-children)
)


;*******************************************************************************************
;Water Jugs Puzzle
;*******************************************************************************************
;This is the state representation for the water jugs puzzle
;the puzzle can be found at http://www.math.tamu.edu/~dallen/hollywood/diehard/diehard.htm
;a state is a list containing 2 elements (4gallon 11gallon)
;each has an integer representing the current volume of water in the container
;4gallon can be any integer from 0 to 4
;11gallon can be any integer from 0 to 11
;start state
;(0 0)
;goal state
;(1 0)
;note that this goal technically means 1 gallon in the 4 gallon jug and 0 in the 11 gallon jug
;but the problem asks for 1 gallon in any jug and any amount in the other.
;This is a trivial difference because once you have one gallon in either jug,
;you can simply empty out the other jug, and then transfer the water
;to the 4 gallon jug

;Function: water-jugs-children
;Description: This functions generates child nodes for the water jugs puzzle
;it takes a state as a parameter and generates the legal child states
;Parameters:
;a state of the water jugs puzzle
;Returns: a list of legal child states
(defun water-jugs-children (state)
    (let ((4gallon (first state)) (11gallon (second state)))
        ;remove any child state that is the same as the parent
        (remove state
            (list
                ;fill up 4 gallon from water source
                (list 4 11gallon)
                ;fill up 11 gallon from water source
                (list 4gallon 11)
                ;empty out 4 gallon
                (list 0 11gallon)
                ;empty out 11 gallon
                (list 4gallon 0)
                ;pour 4 gallon into 11 gallon
                (let ((diff (min (- 11 11gallon) 4gallon)))
                    (list (- 4gallon diff) (+ 11gallon diff))
                )
                ;pour 11gallon into 4 gallon
                (let ((diff (min (- 4 4gallon) 11gallon)))
                    (list (+ 4gallon diff) (- 11gallon diff))
                )
            )
        ;test for removing parent state from children
        :test #'equal)
    )
)

;Function: water-jugs
;Description: This functions returns a list which represents the water jugs puzzle
;where the first item is the start state,
;the second is the goal sate,
;and the third is a function that takes a state and generates the legal child states
;Parameters: none
;Returns: a list containing the game representation
(defun water-jugs ()
    '((0 0) (1 0) water-jugs-children)
)

;*******************************************************************************************
;8-tile puzzle
;*******************************************************************************************
;This is the state representation for the 8-tile puzzle
;the puzzle can be found at http://www.tilepuzzles.com/default.asp?p=12
;a state is a list containing 9 elements
;the position in the lists represents the position on the grid
;and the value of the element represents the tile that is in that position
;a value of 0 is used to represent the blank space
;e.g.
;|6|7|8|
;|3|4|5|
;|0|1|2|
;will be represented as
;(0 1 2 3 4 5 6 7 8)
;Since this puzzle does not have a set start and end state
;you must provide them to the 8-tile function when you use it

;Function: 8-tile-children
;Description: This functions generates child nodes for the 8-tile puzzle
;it takes a state as a parameter and generates the legal child states
;Parameters:
;a state of the 8-tile puzzle
;Returns: a list of legal child states
(defun 8-tile-children (state)
    (let ((x (mod (position 0 state) 3)) (y (/ (- (position 0 state) (mod (position 0 state) 3)) 3)))
        (remove nil (list
            ;swap with tile right
            (cond ((< x 2)
                (swap (position 0 state) (+ (* 3 y) (1+ x))  state)
            ))
            ;swap with tile left
            (cond ((> x 0)
                (swap (position 0 state) (+ (* 3 y) (1- x))  state)
            ))
            ;swap with tile above
            (cond ((< y 2)
                (swap (position 0 state) (+ (* 3 (1+ y)) x)  state)
            ))
            ;swap with tile below
            (cond ((> y 0)
                (swap (position 0 state) (+ (* 3 (1- y)) x)  state)
            ))
        ))
    )
)

;Function: swap
;Description: This functions swaps two items in a list
;note this only works if there are no duplicate values
;Parameters:
;i, the index of the first item to swap
;j, the index of the second item to swap
;l, the list which elements are being swapped
;Returns: a list where the given indexes have been swapped
(defun swap (i j l)
    (mapcar (lambda (x)
        (cond
            ((equal (position x l) i)
                (elt l j)
            )
            ((equal (position x l) j)
                (elt l i)
            )
            (t
                x
            )
        )
    ) l)
)

;Function: 8-tile
;Description: This functions returns a list which represents the 8-tile puzzle
;where the first item is the start state,
;the second is the goal sate,
;and the third is a function that takes a state and generates the legal child states
;Parameters:
;start, a state representing the start state of the puzzle
;goal, a state representing the goal state of the puzzle
;Returns: a list containing the game representation
(defun 8-tile (start goal)
    (list start goal '8-tile-children)
)

;Function: manhattan-distance
;Description: Calculates the manhattan distance between two positions in the 8-tile puzzle
;position is just the position of the element in the list e.g.
;(0 1 2 3 4 5 6 7 8)
;Parameters:
;p1, the first position
;p2, the second position
;Returns: the manhattan distance between the two positions
(defun manhattan-distance (p1 p2)
    (let
        (
            (x1 (mod p1 3))
            (y1 (/ (- p1 (mod p1 3)) 3))
            (x2 (mod p2 3))
            (y2 (/ (- p2 (mod p2 3)) 3))
        )
        (+
            (abs (- x1 x2))
            (abs (- y1 y2))
        )
    )
)

;Function: manhattan-distance-heuristic
;Description: This is a heuristic function for the 8-tile game
;it returns the sum of the manhattan distance of each tile in the puzzle
;from its position in the current state, to the position in the goal state
;this includes 0 which is the blank space
;Parameters:
;state, a state of the 8-tile puzzle
;goal-state, the goal state of the current 8-tile puzzle
;Returns: a number which is the sum of the manhattan distances for the given state to the goal state
(defun manhattan-distance-heuristic (state goal-state)
    (eval
        (cons
            '+
            (mapcar
                (lambda (n)
                    (manhattan-distance (position n state) (position n goal-state))
                )
                state
             )
        )
    )
)

;Function: custom-8-tile-heuristic
;Description: This is a heuristic function for the 8-tile game
;this heuristic is the sum of the following equation for each tile
;the tile's manhattan distance from the state to the goal state
;multiplied by
;the tile's manhattan distance from the tile to the blank tile
;note that because the blank tile's distance to the blank tile is 0
;the blank tile is effectively left out of this sum
;Parameters:
;state, a state of the 8-tile puzzle
;goal-state, the goal state of the current 8-tile puzzle
;Returns: a number which is the sum of the manhattan distances for the given state to the goal state
(defun custom-8-tile-heuristic (state goal-state)
    (eval
        (cons
            '+
            (mapcar
                (lambda (n)
                    (*
                        (manhattan-distance (position n state) (position n goal-state))
                        (manhattan-distance (position n state) (position 0 state))
                    )
                )
                state
             )
        )
    )
)



;easy
;(defparameter start '(1 2 5 3 4 0 6 7 8))
;(defparameter goal  '(0 1 2 3 4 5 6 7 8))

;medium 8 moves
;(defparameter start '(5 2 0 4 6 7 8 1 3))
;(defparameter goal  '(5 6 2 8 0 4 1 3 7))

;hard more than 10
;(defparameter start '(3 5 2 8 6 4 0 1 7))
;(defparameter goal  '(3 4 5 8 0 7 1 2 6))

;impossible
(defparameter start '(7 5 0 4 1 8 6 3 2))
(defparameter goal  '(7 8 0 4 5 6 1 2 3))

(defun test () (custom-8-tile-heuristic start goal))
(defun test8 () (heuristic-search 'custom-8-tile-heuristic (8-tile start goal) 20))
(defun test8- () (heuristic-search 'manhattan-distance-heuristic (8-tile start goal) 20))
