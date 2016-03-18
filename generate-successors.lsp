#|

Comments go here







|#

; start defining functions

(defun generate-successors (state)
	;define local variables
	(let ( (newstate (copy-list state)) (succs nil) )
		
		;if 0 is in top row
		(when (< (position 0 newstate) *MAX*)
			;move left
			(when (not (equal (position 0 newstate) 0))
				(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (1- (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move right
			(when (not (equal (position 0 newstate) (- *MAX* 1)))
				(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (1+ (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move down
			(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (+ (position 0 temp) *MAX*) temp))
					(setf succs (cons temp succs))
			)
		)
		
		;if 0 is in middle row
		(when (and (>= (position 0 newstate) *MAX*) (< (position 0 newstate) (* *MAX* 2)))
			;move left
			(when (not (equal (position 0 newstate) *MAX*))
				(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (1- (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			
			;move right
			(when (not (equal (position 0 newstate) (1- (* *MAX* 2))))
				(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (1+ (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			
			;move up
			(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (- (position 0 temp) *MAX*) temp))
					(setf succs (cons temp succs))
			)
			
			;move down
			(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (+ (position 0 temp) *MAX*) temp))
					(setf succs (cons temp succs))
			)
		)
		
		;if 0 is in bottom row
		(when (>= (position 0 newstate) (* *MAX* 2))
			;move left
			(when (not (equal (position 0 newstate) (* *MAX* 2)))
				(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (1- (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move right
			(when (not (equal (position 0 newstate) (- (* *MAX* 3) 1)))
				(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (1+ (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move up
			(let ((temp (copy-list newstate)))
					(rotatef (nth (position 0 temp) temp) (nth (- (position 0 temp) *MAX*) temp))
					(setf succs (cons temp succs))
			)
		
		)
		
		(return-from generate-successors succs)
	)
)

(defun goal-state? (state)
	(when (equal state nil) (return-from goal-state nil))
	(equal state (list 1 2 3 8 0 4 7 6 5))
)