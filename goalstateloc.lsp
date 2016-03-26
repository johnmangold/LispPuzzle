#|

Comments go here







|#

(defun generate_goalStateLoc (n)
  ;generate goal state
  (let ((n2 (* n n))
		(goalStateLoc nil)
		(row1 0) (row2 (1- n))
		(col1 0) (col2 (1- n))
		(rowC 0) (colC 0)
		(tileC 1)
		(c1 0)
		(direction 'right))
	
	;generate goalStateLoc
	(setf goalStateLoc (make-list n2 :initial-element '(0 0)))
	;set 1 tile
        (if (equal (mod n 2) 0)
          (setf (nth 0 goalStateLoc) (list (floor n 2) (- (floor n 2) 1)))
          (setf (nth 0 goalStateLoc) (list (floor n 2) (floor n 2)))
        )
	;(setf (nth 0 goalStateLoc) '(0 0))
	;loop until all tiles filled in
	(loop
	  ;increment tileC
	  (setf tileC (1+ tileC))
	  ;check if tileC = n2
	  (when (= tileC n2)
	    (return-from generate_goalStateLoc goalStateLoc)
	  )
	  ;move in direction and update direction if needed
	  (cond
	    ((eq direction 'right)
		  ;move right
		  (setf colC (1+ colC))
		  ;set tile
		  (setf (nth tileC goalStateLoc) (list rowC colC))
		  ;check if direction needs to change
		  (when (= colC col2)
		    (setf direction 'down)
			;also mark row finished
			(setf row1 (1+ row1))
		  )
		)
		((eq direction 'left)
		  ;move left
		  (setf colC (1- colC))
		  ;set tile
		  (setf (nth tileC goalStateLoc) (list rowC colC))
		  ;check if direction needs to change
		  (when (= colC col1)
		    (setf direction 'up)
			;also mark row finished
			(setf row2 (1- row2))
		  )
		)
		((eq direction 'up)
		  ;move up
		  (setf rowC (1- rowC))
		  ;set tile
		  (setf (nth tileC goalStateLoc) (list rowC colC))
		  ;check if direction needs to change
		  (when (= rowC row1)
		    (setf direction 'right)
			;also mark row finished
			(setf col1 (1+ col1))
		  )
		)
		((eq direction 'down)
		  ;move down
		  (setf rowC (1+ rowC))
		  ;set tile
		  (setf (nth tileC goalStateLoc) (list rowC colC))
		  ;check if direction needs to change
		  (when (= rowC row2)
		    (setf direction 'left)
			;also mark row finished
			(setf col2 (1- col2))
		  )
		)	
	  )
	)
	(return-from generate_goalStateLoc goalStateLoc)
  )
)
