; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
#|
	Author: Dr.John Weiss
	Description: takes a current node and list of nodes and returns a list of states that are the solution path
	Arguments: node - the current node.  should be the node that matches the goal state.
			   node-list - list of nodes that led to the goal
|#
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
#|
	Author: Dr.John Weiss
	Description: looks for a node on the node-list with the same state
	Arguments: state - list that is the state
			   node-list - list of nodes to check the states if they match with given state.
|#
(defun member-state (state node-list)
    (dolist (node node-list)
        (when (equal state (node-state node)) (return node))
    )
)