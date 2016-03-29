#|
					***** 8PUZZLE.LSP *****
					
Npuzzle Problem:
	Given a square board with one empty space represented by a 0, move
the zero around it till it reaches a decided goal state.  The 0 can move
left, right, up, or down if the move doesn't push it off the board.

Usage: clisp 8puzzle.lsp file.puz
Usage within clisp interpreter: (load '8puzzle) then
								(8puzzle list) or (8puzzle)

Authors: Jacob St.Amand, John Mangold, Jason Anderson
Written Spring 2016 for CSC447 AI class, Assignment 2.

Modifications:
|#

;-------------------------------------------------------------------------------------

;global variables
(defvar *MAX*)		; size of puzzle

;------------- load files -------------
(load 'search_dfid)
(load 'bfsN)
(load 'search_astar)
(load 'print-solution)
(load 'build-solution)
(load 'solvable)

;------------------global variables------------
(defvar *MAX*)

;-------------- helper functions --------------

;listSplit
;convert list format to be list of lists
;each internal list being a row of the puzzle
#|
	Author: Jacob St.Amand
	Description: converts 1d list into a list of lists with each internal list representing a row in the puzzle
	Arguments: inList - starting state in the form of a list ex. (1 2 3 4 5 6 7 8 0)
			   n - square root of length of inList ex if inList is (1 2 3 4 5 6 7 8 0) then n=3
|#
(defun listSplit (inList n)
  (let ((c1 0)
       (c2 0)
	   (c3 0)
	   (tempList nil)
	   (outList nil))
	   
    ;loop through list
	(loop
	  (when (>= c1 n) (return))
	  (setf c2 0)
	  (setf tempList nil)
	  (loop
	    (when (>= c2 n) (return))
		;add to tempList
		(setf tempList (append tempList (list (nth c3 inList))))
		
		(setf c3 (1+ c3))
		(setf c2 (1+ c2))
	  )
	  ;add tempList to outList
	  (setf outList (append outList (list tempList)))
	  (setf c1 (1+ c1))
	)
	
	(return-from listSplit outList)
  )
)

;findZero
;finds zero location of a 1d list
#|
	Author: Jacob St.Amand
	Description: finds the zero location in a 1d list
	Arguments: inList - starting state in the form of a list ex. (1 2 3 4 5 6 7 8 0)
			   n - square root of length of inList ex if inList is (1 2 3 4 5 6 7 8 0) then n=3
|#
(defun findZero (inList n)
  (let ((zeroLoc nil)
       (zeroRow 0)
	   (zeroCol 0)
	   (c1 0)
	   (c2 0)
	   (c3 0))
    ;loop through list
	(loop
	  (when (>= c1 n) (return))
	  (setf c2 0)
	  (setf zeroCol 0)
	  (loop
	    (when (>= c2 n) (return))
		;check if zero
		(when (equal (nth c3 inList) 0)
		  (setf zeroLoc (list zeroRow))
		  (setf zeroLoc (append zeroLoc (list zeroCol)))
		  (return-from findZero zeroLoc)
		)
		
		(setf zeroCol (1+ zeroCol))
		(setf c3 (1+ c3))
		(setf c2 (1+ c2))
	  )
	  (setf zeroRow (1+ zeroRow))
	  (setf c1 (1+ c1))
	)
	
	(if (null zeroLoc)
	  (error "Error: No zero in puzzle!~%")
	  ()
	)
	
	(return-from findZero zeroLoc)
	
  )
)

;main function
#|
	Author: Jacob St.Amand, Jason Anderson, John Mangold
	Description: handles all user input and how it was called either from command line,
				 from within the clisp interpreter with a puzzle filename, or from within
				 the clisp interpreter without a puzzle filename.
	Arguments: &optional inList - starting state in the form of a list ex. (1 2 3 4 5 6 7 8 0)
|#
(defun 8puzzle (&optional inList)
  (when (null inList)
	  ;ask user to input values seperated by whitespace
	  (format t "Please enter start state.~%")
	  (format t "Ex: \'1 2 3 6 5 4 0 8 7\'~%")
	  (format t "This can handle N length puzzles~%")
	  (format t ">>> ")
	  (let ((userInput (read-line)))
		;convert to list
		(setf userInput (concatenate 'string "'(" userInput ")"))
		(setf inList (eval (read-from-string userInput)))
	  )
  )
  
  (let ((n 0)
       (zeroLoc nil))
    (setf n (sqrt (length inList)))
	(setf *MAX* (sqrt (length inList)))
	(when (equal *MAX* 3)
		(cond
			((not (solvable inList))
				(format t "This puzzle is not solvable. ~%The program will exit.~%")
				(return)
			)
		)
	)
	
	;check count
	(if (integerp n)
	  ()
	  (error "Error: Invalid number of input values!~%")
	)
	(setf zeroLoc (findZero inList n))
	(setf inList (listSplit inList n))
	;call search functions
	(setf *MAX* n)
	(format t "~%")
	(bfsN inList zeroLoc n)
	(search_dfid inList zeroLoc n)
	(search_Astar inList zeroLoc n "Hamming")
	(search_Astar inList zeroLoc n "Manhattan")
        (search_Astar inList zeroLoc n "Inadmissible")
  )
)

;---------------- main function ----------------
(cond
  ;test for argument
  ((= (length *ARGS*) 1)
  (with-open-file (inFile (first *ARGS*))
    (let ((inList nil)
	     (inValue nil)
		 (listCount 0))
      ;read from file
	  (loop
	    ;read from file
	    (setf inValue (read inFile nil))
		;return when nil (eof)
		(when (null inValue) (return))
		;add to list
		(setf inList (append inList (list inValue)))
		;increment count
		(setf listCount (1+ listCount))
	  )
	  ;check count
	  (if (integerp (sqrt listCount))
	    ()
		(error "Error: Invalid number of input values!~%")
	  )
	  (let ((n (sqrt listCount))
			(*MAX* (sqrt listCount)))
		(when (equal *MAX* 3)
			(cond
				((not (solvable inList))
					(format t "This puzzle is not solvable. ~%The program will exit.~%")
					(return)
				)
			)
		)
		
	    (setf zeroLoc (findZero inList n))
	    (setf inList (listSplit inList n))
	    ;call search functions
		(setf *MAX* n)
		(format t "~%This program can be used to solve for N sized puzzles.~%")
		(format t "~%")
		(bfsN inList zeroLoc n)
	    (search_dfid inList zeroLoc n)
	    (search_Astar inList zeroLoc n "Hamming")
	    (search_Astar inList zeroLoc n "Manhattan")
	    (search_Astar inList zeroLoc n "Inadmissible")
	  )
	)
  ))
  
  ;else print usage message
  (t 
    (format t "~%---------- Npuzzle.lsp ----------~%")
    (format t "Usage: (clisp Npuzzle.lsp puzzlefile)~%")
	(format t "Usage: (Npuzzle [list])~%")
  )
)

