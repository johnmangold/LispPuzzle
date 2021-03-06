#|

Comments go here







|#

; start defining functions

(defun generate-successors (state)
	;define local variables
	(let ( (top (copy-list (car state))) (mid (copy-list (cadr state))) (bottom (copy-list (caddr state))) (succs nil) )
		
		;if 0 is in top row
		(when (find 0 top :test #'equal)
			;move left
			(when (not (equal (position 0 top) 0))
				(let ((temp (copy-list top)))
					(rotatef (nth (position 0 temp) temp) (nth (1- (position 0 temp)) temp))
					(setf succs (cons (list temp mid bottom) succs))
				)
			)
			;move right
			(when (not (equal (position 0 top) (- *MAX* 1)))
				(let ((temp (copy-list top)))
					(rotatef (nth (position 0 temp) temp) (nth (1+ (position 0 temp)) temp))
					(setf succs (cons (list temp mid bottom) succs))
				)
			)
			;move down
			(let ( (placen (nth (position 0 top) mid)) (temp (copy-list top)) (tempMid (copy-list mid)) )
				(setf (nth (position 0 temp) tempMid) 0)
				(setf (nth (position 0 temp) temp) placen)
				(setf succs (cons (list temp tempMid bottom) succs))
			)
		)
		
		;if 0 is in middle row
		(when (find 0 mid :test #'equal)
			;move left
			(when (not (equal (position 0 mid) 0))
				(let ((temp (copy-list mid)))
					(rotatef (nth (position 0 temp) temp) (nth (1- (position 0 temp)) temp))
					(setf succs (cons (list top temp bottom) succs))
				)
			)
			
			;move right
			(when (not (equal (position 0 mid) (- *MAX* 1)))
				(let ((temp (copy-list mid)))
					(rotatef (nth (position 0 temp) temp) (nth (1+ (position 0 temp)) temp))
					(setf succs (cons (list top temp bottom) succs))
				)
			)
			
			;move up
			(let ( (placen (nth (position 0 mid) top)) (temp (copy-list mid)) (tempTop (copy-list top)) )
				(setf (nth (position 0 temp) tempTop) 0)
				(setf (nth (position 0 temp) temp) placen)
				(setf succs (cons (list tempTop temp bottom) succs))
			)
			
			;move down
			(let ( (placen (nth (position 0 mid) bottom)) (temp (copy-list mid)) (tempBottom (copy-list bottom)) )
				(setf (nth (position 0 temp) tempBottom) 0)
				(setf (nth (position 0 temp) temp) placen)
				(setf succs (cons (list top temp tempBottom) succs))
			)
		)
		
		;if 0 is in bottom row
		(when (find 0 bottom :test #'equal)
			;move left
			(when (not (equal (position 0 bottom) 0))
				(let ((temp (copy-list bottom)))
					(rotatef (nth (position 0 temp) temp) (nth (1- (position 0 temp)) temp))
					(setf succs (cons (list top mid temp) succs))
				)
			)
			;move right
			(when (not (equal (position 0 bottom) (- *MAX* 1)))
				(let ((temp (copy-list bottom)))
					(rotatef (nth (position 0 temp) temp) (nth (1+ (position 0 temp)) temp))
					(setf succs (cons (list top mid temp) succs))
				)
			)
			;move up
			(let ( (placen (nth (position 0 bottom) mid)) (temp (copy-list bottom)) (tempMid (copy-list mid)) )
				(setf (nth (position 0 temp) tempMid) 0)
				(setf (nth (position 0 temp) temp) placen)
				(setf succs (cons (list top tempMid temp) succs))
			)
		
		)
		
		(return-from generate-successors succs)
	)
)

(defun goal-state (state)
	(when (equal state nil) (return-from goal-state nil))
	(equal state (list (list 1 2 3) (list 8 0 4) (list 7 6 5)))
)