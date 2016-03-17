#|

Comments go here







|#

;-------------------------------------------------------------------------------------

;global variables
(defvar *MAX*)		; size of puzzle

;-------------------------------------------------------------------------------------

; load needed files






;-------------------------------------------------------------------------------------

; start defining functions

(defun generate-successors (state)
	;define local variables
	(let ( (top (car state)) (mid (cadr state)) (bottom (caddr state)) (succs nil) 
		pos)
		
		;if 0 is in top row
		(when (find 0 top :test #'equal)
			;move left
			(when (not (equal (position 0 top) 0))
				(let ((temp top))
					(rotatef (nth (position 0 temp) temp) (nth (1- (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move right
			(when (not (equal (position 0 top) (- *MAX* 1)))
				(let ((temp top))
					(rotatef (nth (position 0 temp) temp) (nth (1+ (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move down
		
		)
		
		;if 0 is in middle row
		(when (find 0 mid :test #'equal)
			;move left
			(when (not (equal (position 0 mid) 0))
				(let ((temp middle))
					(rotatef (nth (position 0 temp) temp) (nth (1- (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move right
			(when (not (equal (position 0 mid) (- *MAX* 1)))
				(let ((temp middle))
					(rotatef (nth (position 0 temp) temp) (nth (1+ (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move up
			
			;move down
		
		)
		
		;if 0 is in bottom row
		(when (find 0 bottom :test #'equal)
			;move left
			(when (not (equal (position 0 bottom) 0))
				(let ((temp bottom))
					(rotatef (nth (position 0 temp) temp) (nth (1- (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move right
			(when (not (equal (position 0 bottom) (- *MAX* 1)))
				(let ((temp bottom))
					(rotatef (nth (position 0 temp) temp) (nth (1+ (position 0 temp)) temp))
					(setf succs (cons temp succs))
				)
			)
			;move up
		
		)
	)