(defun flatten (lst)
	(cond
		((null lst) nil)
		((atom lst) (list lst))
		(t (loop for a in lst appending (flatten a)))
	)
)