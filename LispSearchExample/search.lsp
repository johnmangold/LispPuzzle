#|
	Author: Murray Lahood-Burns
	Description: Given state the function will generate all possible
		     successors and does a recursive call.  All returns
		     get appended to the initial list and if the goal is reached
		     it is returned from the function.
	Argument: state
	Returns: path - list containing the states leading to the goal
|#
(defun dfs(state)
	; format start-state into a list
	(when (equal (list "left") (last state))
		(setq state (list state)))

	; base case
	(when (goal-state? (car (last state)))
		(return-from dfs state))
	
	; generate children for current state
	(dolist (child (generate-successors (car (last state))))
		; if child has not already been encountered, add to path
		(unless (find_child child state)
			; depth first search
			(setq path ( dfs (append state (list child))))
			
			; return path if goal is reached
			(when (goal-state? (car (last path)))
				(return-from dfs path))
		)
	)

	; return failure
	nil
)

#|
	Author: Murray Lahood-Burns
	Description: Checks a list of states to see if an entry is already
		     present.
	Arguments: child - the state to check the list for.
		   path - the list containing all the states
	Returns: t if child is found in list, nil if child is not found
|#
(defun find_child (child path)
	; check path if child already exists
	(dolist (i path)
		(when (equal child i)
			(return-from find_child t)
		)
	)
	
	; return nil if not found
	nil
)
