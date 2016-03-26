#|

Comments go here







|#

;-------------------------------------------------------------------------------------

;global variables
(defvar *MAX*)		; size of puzzle

;-------------------------------------------------------------------------------------

; load needed files
(load 'generate-successors)
(load 'search)
(load 'solvable)
(load 'print-solution)




;-------------------------------------------------------------------------------------

; start defining functions
; this is where we'll have our "main" function
(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil
			)
		)
)

(defun 8puzzle (&optional (puzzle nil))
	(cond
		((null puzzle)
			(print "This is when no args were entered or no puzzle was supplied")
			(print "We would handle user input here.")
		)
	
		((listp puzzle)
			(setf *MAX* (sqrt (length puzzle)))
			(print "The user provided a puzzle")
			(print "We would handle calling necessary functions")
			(print-solution (bfs puzzle))
		)
	)

)

(cond
	((> (length *ARGS*) 1)
		(print *ARGS*)
		(print "When calling from command line, you must provide an input filename.")
		(print "The program will now exit.~%")
		(exit)
	)
	
	((equal (length *ARGS*) 1)
		(print *ARGS*)
		(print "Handle with filename")
		(setf in (open (car *ARGS*)))
		(setf top (read-line in))
		(setf mid (read-line in))
		(setf bot (read-line in))
		(close in)
		(setf top (string-to-list top))
		(setf mid (string-to-list mid))
		(setf bot (string-to-list bot))
		(setf puz (append top mid))
		(setf puz (append puz bot))
		(8puzzle puz)
	)
)