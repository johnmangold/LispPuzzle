#|
					***** MISSIONARIES_001.LSP *****
					
Missionary and Cannibal Problem:
	Move all M missionaries and C cannibals from the left side of a river to the
right. Can only use a rowboat that fits a maximum of two people, and if either
shore has 1 or more missionaries out numbered by cannibals, the cannibals will
eat them. Calls DFS search routine in search.lsp, and ascii art in ascii.lsp.

Usage: clisp missionaries
Usage within clisp interpreter: (load 'missionaries)

Authors: John Mangold, Murray LaHood-Burns
Written Spring 2015 for CSC461 PL class, programming assignment 2.

Modifications:
|#

;-------------------------------------------------------------------------------

; global variables
(defvar *M*)		; number of missionaries
(defvar *C*)		; number of cannibals

;-------------------------------------------------------------------------------

; load DFS routine and ascii art from external files
(load 'ascii)
(load 'search)
(load 'print)

;-------------------------------------------------------------------------------

; usage statement displayed at file load time
#|
	Author: John Mangold
	Description: Prints a description and usage for the program.
	Arguments: None
|#
(defun initial_usage ()
	(format t "~%~%This is a recursive solution to the missionaries and ~%")
	(format t "cannibals problem.  It uses a depth first search to solve ~%")
	(format t "the problem.~%~%")
	(format t "Usage: (m-c m c)~%")
	(format t "        m - number of missionaries~%")
	(format t "        c - number of cannibals~%~%")
)

;-------------------------------------------------------------------------------

; Missionary and Cannibal Problem
#|
	Author: Murray Lahood-Burns, John Mangold
	Description: First function to call.  Handles all other function calls.
		     Sets variables then calls a depth first search which returns
		     a list which is then passed to the print function which
		     prints everything out.
	Arguments: m - starting number of missionaries on left bank
		   c - starting number of cannibals on right bank
	Returns: None
|#
(defun m-c (m c)
	; variable check
	(format t "~%There are ~d missionaries and ~d cannibals.~%~%" m c )

	; check for unsolvable problem instance
	(cond
		((not (= m 0))
			(when (< m c) (defeat) (return-from m-c "The cannibals have feasted!!!"))))

	; initialize global vars
	(setf *M* m)
	(setf *C* c)

	; solve missionary/cannibal problem using DFS

	(print_mc (dfs (start-state)))

	; suppress printing NIL upon return to interpreter
	(values)
)

; Define the start state
#|
	Author: John Mangold
	Description: Generates the initial state for the game
	Arguments: None
	Returns: start state list
|#
(defun start-state () (list *M* *C* "left" ) )

; Define the goal state
#|
	Author: John Mangold
	Description: Checks the given state to see if it matches the goal state.
	Arguments: state
	Returns: t if states match, nil if states do not match
|#
(defun goal-state? (state)
	(when (equal state nil) (return-from goal-state? nil))
	(and (= (car state) 0) (= (cadr state) 0) (string= (caddr state) "right"))
)

; Generate-successors returns a list of successors to the current state.
#|
	Author: Murray Lahood-Burns
	Description: Given state it generates every possible successor state.
	Arguments: state
	Returns: succs - list containing the next possible states
		     
|#
(defun generate-successors (state)
	; define local variables
	(let ( (ml (car state)) (cl (cadr state)) (bank (caddr state)) mr cr
		   (succs nil) )
	
		; set number of missionaries and cannibals on right
		(setq mr (- *M* ml) cr (- *C* cl))
	
		; moves if canoe is on left bank
		(when (string= bank "left")
			; 1 missionary right
			(when (and (>= ml 1)
					   (or (>= (- ml 1) cl)
						   (= ml 1))
					   (>= (+ mr 1) cr))
				(setq succs (cons (list (- ml 1) cl "right") succs)))
				
			; 2 missionaries right
			(when (and (>= ml 2)
					   (or (>= (- ml 2) cl)
						   (= ml 2))
					   (>= (+ mr 2) cr))
				(setq succs (cons (list (- ml 2) cl "right") succs)))
				
			; 1 cannibal right
			(when (and (>= cl 1)
					   (or (>= mr (+ cr 1))
						   (= mr 0)))
				(setq succs (cons (list ml (- cl 1) "right") succs)))
				
			; 2 cannibals right
			(when (and (>= cl 2)
					   (or (>= mr (+ cr 2))
						   (= mr 0)))
				(setq succs (cons (list ml (- cl 2) "right") succs)))
				
			; 1 missionary and 1 cannibal right
			(when (and (>= cl 1)
					   (>= ml 1)
					   (>= (+ mr 1)
					   (+ cr 1)))
				(setq succs (cons (list (- ml 1) (- cl 1) "right") succs)))
		)
		
		; moves if canoes is on right bank
		(when (string= bank "right")
			; 1 missionary left
			(when (and (>= mr 1)
					   (or (>= (- mr 1) cr)
						   (= mr 1))
					   (>= (+ ml 1) cl))
				(setq succs (cons (list (+ ml 1) cl "left") succs)))
				
			; 2 missionaries left
			(when (and (>= mr 2)
					   (or (>= (- mr 2) cr)
						   (= mr 2))
					   (>= (+ ml 2) cl))
				(setq succs (cons (list (+ ml 2) cl "left") succs)))
				
			; 1 cannibal left
			(when (and (>= cr 1)
					   (or (>= ml (+ cl 1))
						   (= ml 0)))
				(setq succs (cons (list ml (+ cl 1) "left") succs)))
				
			; 2 cannibal left
			(when (and (>= cr 2)
					   (or (>= ml (+ cl 2))
						   (= ml 0)))
				(setq succs (cons (list ml (+ cl 2) "left") succs)))
			
			; 1 missionary and 1 cannibal left
			(when (and (>= cr 1)
					   (>= mr 1)
					   (>= (+ ml 1) (+ cl 1)))
				(setq succs (cons (list (+ ml 1) (+ cl 1) "left") succs)))
		)
		
		; return list of successors
		(return-from generate-successors succs)
	)
)

;-------------------------------------------------------------------------------

; print usage statement automatically upon loading file
(initial_usage)
