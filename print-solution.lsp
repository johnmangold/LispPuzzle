(defun print-solution (path)
	;will be a double loop
	;outer loop runs through path
	;inner loop will run through each state and print
	;one row at a time. Arrow inbetween each.
	;inner loop will run *MAX* times. one for each row.
	;it will print three states across until all states are printed
	(setf count 1)
	(setf cont 0)
	(setf solcont 0)
	(setf sollength (length path))
	(setf toprint 4)
	(format t "~%")
	(dotimes (num (ceiling (length path) 4))
		(dotimes (rows *MAX*)
			(dotimes (i toprint)
				(dotimes (j *MAX*)
					(cond 
						((and (equal j (- *MAX* 1)) (or (equal rows 0) (equal rows (- *MAX* 1))))
							(format t "~d      " (nth (+ j cont) (nth (+ i solcont) path)))
						)
						
						((and (equal j (- *MAX* 1)) (equal (- (length path) 1) (+ i solcont)))
							(format t "~d      " (nth (+ j cont) (nth (+ i solcont) path)))
						)
						
						((and (equal j (- *MAX* 1)) (not (equal (- (length path) 1) (+ i solcont))))
							(format t "~d  ->  " (nth (+ j cont) (nth (+ i solcont) path)))
						)
						
						( t (format t "~d " (nth (+ j cont) (nth (+ i solcont) path))))
					)
				)
			)
			(format t "~%")
			(setf cont (+ cont *MAX*))
		)
		(format t "~%~%")
		(setf solcont (+ solcont 4))
		(setf cont 0)
		(setf sollength (- sollength toprint))
		(when (< sollength toprint)
			(setf toprint sollength)
		)
	)
)


(defun print-solution2 (path)
	;will be a double loop
	;outer loop runs through path
	;inner loop will run through each state and print
	;one row at a time. Arrow inbetween each.
	;inner loop will run *MAX* times. one for each row.
	;it will print three states across until all states are printed
	(let ((count 1)
	     (cont 0)
	     (solcont 0)
	     (sollength (length path))
	     (toprint 4)
	     (tempVal 0))
	(format t "~%")
	(if (< sollength 4)
	    (setf toprint sollength))
	(dotimes (num (ceiling (length path) 4))
		(dotimes (rows *MAX*)
			(dotimes (i toprint)
				(dotimes (j *MAX*)
				    (setf tempVal (nth (mod (+ j cont) *MAX*) (nth (floor (+ j cont) *MAX*) (nth (+ i solcont) path))))
					(cond 
						((and (equal j (- *MAX* 1)) (or (equal rows 0) (equal rows (- *MAX* 1))))
						    (if (= tempVal 0)
							  (format t "        ")
							  (format t "~2d      " tempVal)
							)
						)
						
						((and (equal j (- *MAX* 1)) (equal (- (length path) 1) (+ i solcont)))
							(if (= tempVal 0)
							  (format t "        ")
							  (format t "~2d      " tempVal)
							)
						)
						
						((and (equal j (- *MAX* 1)) (not (equal (- (length path) 1) (+ i solcont))))
						    (if (= tempVal 0)
							  (format t "    ->  ")
							  (format t "~2d  ->  " tempVal)
							)
						)
						
						( t 
						    (if (= tempVal 0)
							  (format t "   ")
							  (format t "~2d " tempVal)
							)
						)
					)
				)
			)
			(format t "~%")
			(setf cont (+ cont *MAX*))
		)
		(format t "~%~%")
		(setf solcont (+ solcont 4))
		(setf cont 0)
		(setf sollength (- sollength toprint))
		(when (< sollength toprint)
			(setf toprint sollength)
		)
	))
)