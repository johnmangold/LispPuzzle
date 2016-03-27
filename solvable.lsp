#|
                  ***** SOLVABLE.LSP *****

The SOLVABLE function returns T if a given 8-puzzle position is solvable,
NIL otherwise.

Usage:    (solvable L)
          where L is a 9-element list such as (1 2 3 8 0 4 7 6 5)

Reference:  "Mathematical Games and Pastimes", p.79-85,
             A.P.Domoryad, Macmillan, 1964.

Written 03/88 by John M. Weiss, Ph.D.

Modifications:
|#

(defvar *flag*)

#|
	Author: Dr.John Weiss
	Description: checks if an 8puzzle is solvable or not
	Arguments: L - 1d list representing starting state of puzzle
|#
(defun solvable (L)
    (setf *flag* nil)                               ; global *flag*
    (mapcar #'(lambda (elem) (disorder elem L)) L)
    (eq *flag* (evenp (position 0 L)))
)

#|
	Author: Dr.John Weiss
	Description: rearranges elements in list
	Arguments: elem - single element
			   L - 1d lilst representing starting state of puzzle
|#
(defun disorder (elem L)
    (cond
        ((eq (car L) elem))
        ((> (car L) elem)
            (setf *flag* (not *flag*))
            (disorder elem (cdr L))
        )
        (t (disorder elem (cdr L)))
    )
)
