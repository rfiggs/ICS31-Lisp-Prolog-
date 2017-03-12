;This file implements the breadth first and depth first search algorithms
;It also implements three logic puzzles which the algorithms can search over
;The farmer, the wolf, the goat, and the cabbage
;The water jugs puzzle
;and the 8 tile puzzle
;to run one of the search algorithms you can simply call
;(search-method (puzzle))
;for example if you want to run the breadth first search on the water jugs puzzle
;(breadth-first (water-jugs))
;note that the 8 tile game needs a start state and an end state to work
;for example
;(depth-first (8-tile '(0 1 2 3 4 5 6 7 8) '(8 7 6 5 4 3 2 1 0)))
;see the 8-tile description below for more details

;the open list used by the search algorithms
(defvar *open* nil)

;the closed list used by the search algorithms
(defvar *closed* nil)

;Function: top-state
;Description: retrieves the state of the top node on the given list
;Parameters:
;node-list: the list of nodes from which to get the top state
;Return: state of the top node on the list
(defun top-state (node-list)
    (car (car node-list))
)

;Function: make-children
;Description: this function returns a list of nodes where each nodes state
;is generated by the given function and the parent state is the given state.
;this function will also remove any states that are already in the *open* or *closed* lists
;Parameters:
;parent-state: this is the state that is passed to the generator function
;it is also used as the parent state for the nodes
;moves: a function that takes a single state as a parameter
;and returns a list of child states
;Return: a list of child nodes for the given state
(defun make-children (parent-state moves)
    (mapcar
        (lambda (x) (list x parent-state))
         (remove-if
             (lambda (x) (or
                (member x (mapcar #'car *open*) :test #'equal)
                (member x (mapcar #'car *closed*) :test #'equal)
            ))
            (remove nil (eval  (list moves (list 'quote parent-state))))
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
        ((format t "~&Solution Path: ~a" path))
        (t
            path
        )
    )

)

;Function: breadth-first-search
;Description: uses the global *open* and *closed* lists
;to find a path to the goal from the start using the breadth first search strategy
;Parameters:
;goal, the target state that we are trying to achieve
;iteration, the current number of nodes looked at
;moves, a function that takes a state as a parameter and  returns the child states
(defun breadth-first-search (goal iteration moves)
    (cond
        ((>= 0 (length *open*))
            nil
        )
        ((print-node (car *open*)))
        ((equal goal (top-state *open*))
            (car *open*)
        )
        (T
            (and
                (setf *closed* (cons (car *open*) *closed*))
                (setf *open* (append
                    (cdr *open*)
                    (make-children (top-state *open*) moves)
                ))
                (breadth-first-search goal (+ 1 iteration) moves)
            )
        )
    )
)

;Function: breadth-first
;Description: sets up the open and closed lists then calls
;the recursive function breadth-first-search to find a path to the goal state
;Parameters:
;game, is a list with three elements where
;(first game), is the start state
;(second game), is the goal state
;(third game), is a function that takes a state and returns its valid children.
;Returns:
;the path from the start state to goal if it exists, otherwise nil
(defun breadth-first (game)
    (let ((start (first game)) (goal (second game)) (moves (third game)))
        (setf *open* (list (list start nil)))
        (setf *closed* nil)
        (print-solution (solution-path (breadth-first-search goal 0 moves)))
    )
)

;Function: depth-first-search
;Description: uses the global *open* and *closed* lists
;to find a path to the goal from the start using the depth first search strategy
;Parameters:
;goal, the target state that we are trying to achieve
;iteration, the current number of nodes looked at
;moves, a function that takes a state as a parameter and  returns the child states
(defun depth-first-search (goal iteration moves)
    (cond
        ((>= 0 (length *open*))
            nil
        )
        ((print-node (car *open*)))
        ((equal goal (top-state *open*))
            (car *open*)
        )
        (T
            (setf *closed* (cons (car *open*) *closed*))
            (setf *open*
                (append
                    (make-children (top-state *open*) moves)
                    (cdr *open*)
                )
            )
            (depth-first-search goal (+ 1 iteration) moves)
        )
    )
)


;Function: depth-first
;Description: sets up the open and closed lists then calls
;the recursive function breadth-first-search to find a path to the goal state
;Parameters:
;game, is a list with three elements where
;(first game), is the start state
;(second game), is the goal state
;(third game), is a function that takes a state and returns its valid children.
;Returns:
;the path from the start state to goal if it exists, otherwise nil
(defun depth-first (game)
    (let ((start (first game)) (goal (second game)) (moves (third game)))
        (setf *open* (list (list start nil)))
        (setf *closed* nil)
        (print-solution (solution-path (depth-first-search goal 0 moves)))
    )
)
;*******************************************************************************************
;Farmer Wolf Goat Cabbage Puzzle
;*******************************************************************************************
;This is the state representation for the farmer, the wolf, the goat and the cabbage puzzle.
;the puzzle can be found at http://www.mathsisfun.com/puzzles/farmer-crosses-river.html
;a state is a list containing 4 elements (wolf goat cabbage near-shore)
;all four items are either t or nil
;wolf, goat, and cabbage, are t when the farmer is on the same side of the river as them otherwise nil
;near-shore is true if the farmer is on the near shore otherwise false when the farmer is on the far shore
;start state
;(t t t t)
;goal state
;(t t t nil)

;Function: fwgc-children
;Description: This functions generates child nodes for the farmer, wolf, goat, and cabbage puzzle
;it takes a state as a parameter and generates the legal child states
;Parameters:
;a state of the fwgc puzzle
;Returns: a list of legal child states
(defun fwgc-children (state)
    (let ((wolf (first state)) (goat (second state)) (cabbage (third state)) (next-shore (not (fourth state)) ))
        ;this will remove the illegal states
        (remove-if (lambda (x) (not (or (second x) (and (first x) (third x)))))
            (remove nil (list
                ;farmer does not taking anything
                (list (not wolf) (not goat) (not cabbage) next-shore)
                ;farmer takes wolf if has wolf
                (cond (wolf
                    (list  wolf (not goat) (not cabbage) next-shore)
                ))
                ;farmer takes goat if has goat
                (cond (goat
                    (list (not wolf) goat (not cabbage) next-shore)
                ))
                ;farmer takes cabbage if has cabbage
                (cond (cabbage
                    (list (not wolf) (not goat) cabbage next-shore)
                ))
            ))
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
            ;swap with tile above
            (cond ((< x 2)
                (swap (position 0 state) (+ (* 3 y) (1+ x))  state)
            ))
            ;swap with tile below
            (cond ((> x 0)
                (swap (position 0 state) (+ (* 3 y) (1- x))  state)
            ))
            ;swap with tile right
            (cond ((< y 2)
                (swap (position 0 state) (+ (* 3 (1+ y)) x)  state)
            ))
            ;swap with tile left
            (cond ((> y 0)
                (swap (position 0 state) (+ (* 3 (1- y)) )  state)
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
