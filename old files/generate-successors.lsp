(defun generate-successors (state stateZloc n)
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
    (return-from generate-successors succs)
  )
)