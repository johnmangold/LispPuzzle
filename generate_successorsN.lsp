#|
	Author: Jacob St.Amand
	Description: given a starting state it generates all possible states and returns a list of possible states.
				 the possible moves are moving the zero up, down, left, and right if the move is possible.
	Arguments: inList - starting state in the form of a list ex. (1 2 3 4 5 6 7 8 0)
			   stateZloc - position of the zero in inList
			   n - square root of length of inList ex if inList is (1 2 3 4 5 6 7 8 0) then n=3
|#
;returns list of (child childZpos)
(defun generate_successorsN (state stateZloc n)
  ;define local variables
  (let ((row (first stateZloc))
	   (col (second stateZloc))
	   (succs nil))
	;(format t "state: ~S~%" state)
	;(format t "stateZloc: ~S~%" stateZloc n)
	;(format t "n: ~S~%" n)
    ;move down
	(when (>= (1- row) 0)
      (let* ((tempList (copy-list state))
	        (tempRow1 (copy-list (nth row tempList)))
			(tempRow2 (copy-list (nth (1- row) tempList))))
	    (rotatef (nth  col tempRow1) (nth col tempRow2))
		(setf (nth row tempList) tempRow1)
		(setf (nth (1- row) tempList) tempRow2)
		(setf succs (cons (list tempList (list (1- row) col)) succs))
      )	  
	)
	
	;move up
	(when (< (1+ row) n)
	  (let* ((tempList (copy-list state))
	        (tempRow1 (copy-list (nth row tempList)))
			(tempRow2 (copy-list (nth (1+ row) tempList))))
	    (rotatef (nth  col tempRow1) (nth col tempRow2))
		(setf (nth row tempList) tempRow1)
		(setf (nth (1+ row) tempList) tempRow2)
		(setf succs (cons (list tempList (list (1+ row) col)) succs))
      )	 
	)
	
	;move left
	(when (>= (1- col) 0)
	  (let* ((tempList (copy-list state))
	        (tempRow (copy-list (nth row tempList))))
	    (rotatef (nth  col tempRow) (nth (1- col) tempRow))
		(setf (nth row tempList) tempRow)
		(setf succs (cons (list tempList (list row (1- col))) succs))
      )	 
	)
	
	;move right
	(when (< (1+ col) n)
	  (let* ((tempList (copy-list state))
	        (tempRow (copy-list (nth row tempList))))
	    (rotatef (nth  col tempRow) (nth (1+ col) tempRow))
		(setf (nth row tempList) tempRow)
		(setf succs (cons (list tempList (list row (1+ col))) succs))
      )	 
	)
		
	;(format t "succs~S~%~%" succs)
    (return-from generate_successorsN succs)
  )
)

#|
	Author: Jacob St.Amand
	Description: given a puzzle size, this generates what the goal state should look like ex. (1 2 3 8 0 4 7 6 5)
	Arguments: n - square root of length of puzzle list ex if puzzle is (1 2 3 4 5 6 7 8 0) then n=3
|#
(defun generate_goal_stateN (n)
  ;generate goal state
  (let ((n2 (* n n))
		(goalState nil)
		(row1 0) (row2 (1- n))
		(col1 0) (col2 (1- n))
		(rowC 0) (colC 0)
		(tileC 1)
		(c1 0)
		(direction 'right))
	
	;generate goalState
	(loop
	  (setf goalState (append goalState (list (make-list n :initial-element 0))) )
	  (setf c1 (1+ c1))
	  (when (= c1 n)
	    (return)
	  )
	)
	;set 1 tile
	(setf (nth 0 (nth 0 goalState)) 1)
	;loop until all tiles filled in
	(loop
	  ;increment tileC
	  (setf tileC (1+ tileC))
	  ;check if tileC = n2
	  (when (= tileC n2)
	    (return-from generate_goal_stateN goalState)
	  )
	  ;move in direction and update direction if needed
	  (cond
	    ((eq direction 'right)
		  ;move right
		  (setf colC (1+ colC))
		  ;set tile
		  (setf (nth colC (nth rowC goalState)) tileC)
		  ;check if direction needs to change
		  (when (= colC col2)
		    (setf direction 'down)
			;also mark row finished
			(setf row1 (1+ row1))
		  )
		)
		((eq direction 'left)
		  ;move right
		  (setf colC (1- colC))
		  ;set tile
		  (setf (nth colC (nth rowC goalState)) tileC)
		  ;check if direction needs to change
		  (when (= colC col1)
		    (setf direction 'up)
			;also mark row finished
			(setf row2 (1- row2))
		  )
		)
		((eq direction 'up)
		  ;move right
		  (setf rowC (1- rowC))
		  ;set tile
		  (setf (nth colC (nth rowC goalState)) tileC)
		  ;check if direction needs to change
		  (when (= rowC row1)
		    (setf direction 'right)
			;also mark row finished
			(setf col1 (1+ col1))
		  )
		)
		((eq direction 'down)
		  ;move right
		  (setf rowC (1+ rowC))
		  ;set tile
		  (setf (nth colC (nth rowC goalState)) tileC)
		  ;check if direction needs to change
		  (when (= rowC row2)
		    (setf direction 'left)
			;also mark row finished
			(setf col2 (1- col2))
		  )
		)
		
	  )
	)
	
	
	(return-from generate_goal_stateN goalState)
  )
)