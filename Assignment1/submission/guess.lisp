;Comments by: Bobby Figgs
;File: guess.lisp
;Think of a number between 1 and 100 and this program will try to guess it.
;Start by thinking of a number between 1 and 100, then call (guess-my-number).
;The computer will then come up with a guess.
;If your number is lower than what is shown call (smaller).
;If your number is bigger than what is shown call (bigger).
;Eventually the computer will figure out your number
;To reset the game call (start-over).

;small is the lower bound of the guess, initially 1
(defparameter *small* 1)
;big is the upper bound of the guess, initiall 100
(defparameter *big* 100)

;guess-my-number() returns the number halfway between small and big
(defun guess-my-number ()
     (ash (+ *small* *big*) -1))

;smaller() sets the upper bound to the current guess minus 1
(defun smaller ()
     (setf *big* (1- (guess-my-number)))
     (guess-my-number))

;bigger() sets the lower bound to the current guess plus one
(defun bigger ()
     (setf *small* (1+ (guess-my-number)))
     (guess-my-number))

;start-over() resets the guessing game
(defun start-over ()
   (defparameter *small* 1)
   (defparameter *big* 100)
   (guess-my-number))