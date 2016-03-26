(defun print-solution (path)
	;will be a double loop
	;outer loop runs through path
	;inner loop will run through each state and print
	;one row at a time. Arrow inbetween each.
	;inner loop will run *MAX* times. one for each row.
	;it will print three states across until all states are printed
	(let (
			(count 1)
			(cont 0)
			(solcont 0)
			(sollength (length path))
			(toprint 4)
			(flatpath (copy-list path))
		)
		
		(dolist (state flatpath)
			(setf (nth (position state flatpath) flatpath) (flatten state))
		)
		
		(format t "~%")
		(dotimes (num (ceiling (length flatpath) 4))
			(dotimes (rows *MAX*)
				(dotimes (i toprint)
					(dotimes (j *MAX*)
						(cond 
							((and (equal j (- *MAX* 1)) (or (equal rows 0) (equal rows (- *MAX* 1))))
								(format t "~d      " (nth (+ j cont) (nth (+ i solcont) flatpath)))
							)
							
							((and (equal j (- *MAX* 1)) (equal (- (length flatpath) 1) (+ i solcont)))
								(format t "~d      " (nth (+ j cont) (nth (+ i solcont) flatpath)))
							)
							
							((and (equal j (- *MAX* 1)) (not (equal (- (length flatpath) 1) (+ i solcont))))
								(format t "~d  ->  " (nth (+ j cont) (nth (+ i solcont) flatpath)))
							)
							
							( t (format t "~d " (nth (+ j cont) (nth (+ i solcont) flatpath))))
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
)