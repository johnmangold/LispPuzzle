#|



|#

;--------------------------------------------------------------------------

; Node structure: stores state and parent.
(defstruct node state stateZloc stateDepth heuristic parent)

; Test if two nodes have the same state.
(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))

;--------------------------------------------------------------------------

(load 'generate_successorsN)
(load 'print-solution)

;--------------------------------------------------------------------------

(defun search_dfid (inList zeroLoc n)
  (let ((searchDepth 1)
       (goalState (generate_goal_stateN n))
	   (nodesGenerated 0)
	   (nodesPlacedOpen 0)
	   (nodesPlacedClosed 0))
    (loop 
	;loop1 - search depth for dfid
	  ;(format t "searchDepth: ~S~%" searchDepth)
	  (let* ((curNode (make-node :state inList :stateZloc zeroLoc :stateDepth 0 :parent nil))
        (OPEN (list curNode))
	    (CLOSED nil)
		(maxSearchDepth nil)
		(solutionPath nil))
        (loop
		
		  ;get current node from OPEN, update OPEN and CLOSED
		  (setf curNode (car OPEN))
		  (setf OPEN (cdr OPEN))
		  (setf CLOSED (cons curNode CLOSED))
		  
		  ;check goal state
		  ;(format t "curNode: ~S~%"  (node-state curNode))
	      (when (equal (node-state curNode) goalState)
		    (setf solutionPath (build-solution curNode CLOSED))
		    (format t "DFID graph search~%")
			(format t "-----------------~%")
			(format t "Solution found in ~S moves~%" (1- (length solutionPath)))
			(format t "~S nodes generated (~S 'distinct' nodes), ~S nodes expanded~%"
			    nodesGenerated nodesPlacedOpen nodesPlacedClosed)
		    (print-solution2 solutionPath)
			(return-from search_dfid t)
		  )
		  
		  ;check if too deep
		  (when (< (node-stateDepth curNode) searchDepth)
            ;increment nodesPlacedClosed
            (setf nodesPlacedClosed (1+ nodesPlacedClosed))			
		    ;generate successors
		    (dolist (child (generate_successorsN (node-state curNode) (node-stateZloc   curNode) n))
		      ;increment nodesGenerated
			  (setf nodesGenerated (1+ nodesGenerated))
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
				;increment nodesPlacedOpen
				(setf nodesPlacedOpen (1+ nodesPlacedOpen))
				
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

#|; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
(defun build-solution (node node-list)
    (do
        ((path (list (node-state node))))        ; local loop var
        ((null (node-parent node)) path)         ; termination condition

        ; find the parent of the current node
        (setf node (member-state (node-parent node) node-list))

        ; add it to the path
        (setf path (cons (node-state node) path))
    )
)

; Member-state looks for a node on the node-list with the same state.
(defun member-state (state node-list)
    (dolist (node node-list)
        (when (equal state (node-state node)) (return node))
    )
)|#
