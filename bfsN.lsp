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
  (let ((goalState (generate_goal_stateN n))
		(nodesGenerated 0)
		(nodesPlacedOpen 0)
		(nodesPlacedClosed 0))
	  (let* ((curNode (make-node :state inList :stateZloc zeroLoc :parent nil))
			(OPEN (list curNode))
			(CLOSED nil))
			(loop
			
				  ;get current bnode from OPEN, update OPEN and CLOSED
				  (setf curNode (car OPEN))
				  (setf OPEN (cdr OPEN))
				  (setf CLOSED (cons curNode CLOSED))
				  
				  ;check goal state
				  ;(format t "curNode: ~S~%"  (node-state curNode))
				  (when (equal (node-state curNode) goalState)
						(setf solutionPath (build-solution curNode CLOSED))
						(format t "BFS graph search~%")
						(format t "-----------------~%")
						(format t "Solution found in ~S moves~%" (1- (length solutionPath)))
						(format t "~S nodes generated (~S distinct nodes), ~S nodes expanded~%"
							nodesGenerated nodesPlacedOpen nodesPlacedClosed)
						(print-solution2 solutionPath)
						(return-from bfsN t)
					)
				  
				(setf nodesPlacedClosed (1+ nodesPlacedClosed))
				;generate successors
				(dolist (child (generate_successorsN (node-state curNode) (node-stateZloc   curNode) n))
				
					(setf nodesGenerated (1+ nodesGenerated))
				  ;for each child bnode
				  (setf child (make-node :state (first child) 
									   :stateZloc (second child)
									   :parent (node-state curNode)))
				
				  ;check if already on OPEN or CLOSED
				  (when (and (not (member child OPEN :test #'bequal-states))
							 (not (member child CLOSED :test #'bequal-states)))
					;add to OPEN list
					(setf OPEN (append OPEN (list child)))
					(setf nodesPlacedOpen (1+ nodesPlacedOpen))
				  )
				
				)
			)
	   )
	)
)