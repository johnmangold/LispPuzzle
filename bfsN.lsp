#|



|#

;--------------------------------------------------------------------------

; bnode structure: stores state and parent.
(defstruct bbnode state stateZloc parent)

; Test if two bnodes have the same state.
(defun bequal-states (n1 n2) (equal (node-state n1) (node-state n2)))

;--------------------------------------------------------------------------

(load 'generate_successorsN)

;--------------------------------------------------------------------------


(defun bfsN (inList zeroLoc n)
  (let ((goalState (generate_goal_stateN n)))
	  (let* ((curNode (make-node :state inList :stateZloc zeroLoc :parent nil))
			(OPEN (list curNode))
			(CLOSED nil))
			(loop
			
			  ;get current bnode from OPEN, update OPEN and CLOSED
			  (setf curNode (car OPEN))
			  (setf OPEN (cdr OPEN))
			  (setf CLOSED (cons curNode CLOSED))
			  
			  ;check goal state
			  (format t "curNode: ~S~%"  (node-state curNode))
			  (when (equal (node-state curNode) goalState)
				(print-solution (build-solution curNode CLOSED))
				(return-from bfsN t)
			  )
			  
			;generate successors
			(dolist (child (generate_successorsN (node-state curNode) (node-stateZloc   curNode) n))
			
			  ;for each child bnode
			  (setf child (make-node :state (first child) 
								   :stateZloc (second child)
								   :parent (node-state curNode)))
			
			  ;check if already on OPEN or CLOSED
			  (when (and (not (member child OPEN :test #'bequal-states))
						 (not (member child CLOSED :test #'bequal-states)))
				;add to OPEN list
				(setf OPEN (append OPEN (list child)))
			  )
			
			)
			)
	   )
	)
)