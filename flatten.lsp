#|
	Author: John Mangold
	Description: takes a list that may have nested lists and flattens it into a 1d list
	Arguments: lst - the list to be flattened into 1d list
|#
(defun flatten (lst)
	(cond
		((null lst) nil)
		((atom lst) (list lst))
		(t (loop for a in lst appending (flatten a)))
	)
)