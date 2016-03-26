#|



|#

;--------------------------------------------------------------------------

; Node structure: stores state and parent.
(defstruct node state stateZloc stateDepth parent)

; Test if two nodes have the same state.
(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))

;--------------------------------------------------------------------------

(load 'generate_successorsN)

;--------------------------------------------------------------------------

(defun search_dfid (inList zeroLoc n)
  (let ((searchDepth 1)
       (goalState (generate_goal_stateN n)))
    (loop 
	;loop1 - search depth for dfid
	  (format t "searchDepth: ~S~%" searchDepth)
	  (let* ((curNode (make-node :state inList :stateZloc zeroLoc :stateDepth 0 :parent nil))
        (OPEN (list curNode))
	    (CLOSED nil)
		(maxSearchDepth nil))
        (loop
		
		  ;get current node from OPEN, update OPEN and CLOSED
		  (setf curNode (car OPEN))
		  (setf OPEN (cdr OPEN))
		  (setf CLOSED (cons curNode CLOSED))
		  
		  ;check goal state
		  (format t "curNode: ~S~%"  (node-state curNode))
	      (when (equal (node-state curNode) goalState)
		    (print-solution (build-solution curNode CLOSED))
			(return-from search_dfid t)
		  )
		  
		  ;check if too deep
		  (when (< (node-stateDepth curNode) searchDepth) 
		    ;generate successors
		    (dolist (child (generate_successorsN (node-state curNode) (node-stateZloc   curNode) n))
		    
			  ;for each child node
			  (setf child (make-node :state (first child) 
			                       :stateZloc (second child)
								   :stateDepth (1+ (node-stateDepth curNode))
								   :parent (node-state curNode)))
			
			  ;check if already on OPEN or CLOSED
			  (when (and (not (member child OPEN :test #'equal-states))
			             (not (member child CLOSED :test #'equal-states)))
			    ;add to OPEN list
			    (setf OPEN (cons child OPEN))
			  )
			
		      )
		    )
			;flag that we need to increment searchDepth
			(when (>= (node-stateDepth curNode) searchDepth)
			  (setf maxSearchDepth t)
			)
			
			;check if search exausted
		    (when (null OPEN)
		      ;check if searchDepth was reached
		      (when (null maxSearchDepth)
			    ;increasing search depth will have no effect
			    (format t "No solution found!~%")
			    (return-from search_dfid nil)
			  )
			  ;break from current loop
			  ;(will increment searchDepth and search again)
			  (return)
		    )
	    )
	
      )
	  ;increment searchDepth and run search again
	  (setf searchDepth (1+ searchDepth))
	;end loop1 - search depth for dfid
	)
  )
  
  
  
)
