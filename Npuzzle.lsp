#|

Comments go here



|#
;-------------------------------------------------------------------------------------

;global variables
(defvar *MAX*)		; size of puzzle

;------------- load files -------------
(load 'search_dfid)

;-------------- helper functions --------------

;listSplit
;convert list format to be list of lists
;each internal list being a row of the puzzle
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
(defun Npuzzle (&optional inList)
  (when (null inList)
	  ;ask user to input values seperated by whitespace
	  (format t "Please enter start state.~%")
	  (format t "Ex: \'1 2 3 6 5 4 0 8 7\'~%")
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
	;check count
	(if (integerp n)
	  ()
	  (error "Error: Invalid number of input values!~%")
	)
	;output list
	;(format t "~S~%" inList)
	(setf zeroLoc (findZero inList n))
	(setf inList (listSplit inList n))
	;(format t "~S~%" inList)
	;call search functions
	(setf *MAX* n)
	(format t "~%")
	;(search_bfs inList zeroLoc n)
	(search_dfid inList zeroLoc n)
	;(search_Astar inList zeroLoc n)
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
	  (let ((n (sqrt listCount)))
	    ;output list
	    ;(format t "~S~%" inList)
	    (setf zeroLoc (findZero inList n))
	    (setf inList (listSplit inList n))
	    ;(format t "~S~%" inList)
	    ;call search functions
		(setf *MAX* n)
		(format t "~%")
	    ;(search_bfs inList zeroLoc n)
	    (search_dfid inList zeroLoc n)
	    ;(search_Astar inList zeroLoc n)
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

