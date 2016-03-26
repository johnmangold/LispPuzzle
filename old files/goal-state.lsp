(load 'flatten)

(defun gen-goal-state (n)
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
	    (return-from goal-state? (flatten goalState))
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
	
	
	(return-from goal-state? (flatten goalState))
  )
)

(defun goal-state? (state)
	(equal state (gen-goal-state *MAX*))
)