;-------------------------------------------------------------------

(load 'generate_successorsN)
(load 'goalStateLoc)

;-------------------------------------------------------------------

#|
	Author: Jason Anderson
	Description: performs an A* search with two admissable heuristics and one inadmissable heuristic
	Arguments: inList - starting state in the form of a list ex. (1 2 3 4 5 6 7 8 0)
			   zeroLoc - position of the zero in inList
			   n - square root of length of inList ex if inList is (1 2 3 4 5 6 7 8 0) then n=3
			   heuristic - name of the heuristic to use
|#
(defun search_Astar (inList zeroLoc n heuristic)
  (let ((goalState (generate_goal_stateN n))
       (nodesGenerated 0)
       (nodesPlacedOpen 0)
       (nodesPlacedClosed 0))
    (setf goalState (generate_goal_stateN n))
    (loop
      (let* ((curNode (make-node :state inList :stateZloc zeroLoc :stateDepth 0 :heuristic 0 :parent nil))
        (OPEN (list curNode))(CLOSED nil) (solutionPath nil))

        (loop
          ;get current node from OPEN, update OPEN and CLOSED
          (setf curNode (car OPEN))
          (setf OPEN (cdr OPEN))
          (setf CLOSED (cons curNode CLOSED))
          (setf nodesPlacedClosed (1+ nodesPlacedClosed))

          ;check goal state
          (when (equal (node-state curNode) goalState)
            (setf solutionPath (build-solution curNode CLOSED))
            (format t "Astar graph search ~S~%" heuristic)
            (format t "---------------------------------~%")
            (format t "Solution found in ~S moves ~%" (1- (length solutionPath)))
            (format t "~S nodes generated (~S distinct nodes), ~S nodes expanded ~%"
              nodesGenerated nodesPlacedOpen nodesPlacedClosed)
                (print-solution2 solutionPath)
                  (return-from search_Astar t)
          )

          ;generate successors
          (dolist (child (generate_successorsN (node-state curNode)(node-stateZloc curNode) n))
            ;for each child node
            (setf nodesGenerated (1+ nodesGenerated))
            (setf child (make-node :state (first child) 
                                   :stateZloc (second child)
                                   :stateDepth (1+ (node-stateDepth curNode))
                                   :heuristic (chooseHeuristic heuristic child goalState n (node-stateDepth curNode))
                                   :parent (node-state curNode)))
            ;check if already on OPEN or CLOSED
            (when (and (not (member child OPEN :test #'equal-states))
                       (not (member child CLOSED :test #'equal-states)))
              ;put into open if not already in OPEN or CLOSED
              (setf OPEN (cons child OPEN))
              (setf nodesPlacedOpen (1+ nodesPlacedOpen))
            )    
          )
          ;put the lowest heuristic first
          (let (lowest heuristicCount openList storeLowest clear)
            (setf heuristicCount 0)
            (setf openList OPEN)
            (setf clear '())
            (setf OPEN clear)
            (dolist (car openList)
              (cond
                ((equal heuristicCount 0)
                  (cond
                    ((not (member (car openList) CLOSED :test #'equal-states))
                      (setf lowest (node-heuristic (car openList))) (setf storeLowest (car openList)))))
                ((< (node-heuristic (car openList)) lowest) 
                  (cond
                    ((not (member (car openList) CLOSED :test #'equal-states))
                    (setf lowest (node-heuristic (car openList))) (setf OPEN (cons storeLowest OPEN)) (setf storeLowest (car openList)))))
                (t 
                  (cond
                    ((not (member (car openList) CLOSED :test #'equal-states))
                    (setf OPEN (cons (car openList) OPEN)))))
              )

              (setf OPEN (cons storeLowest OPEN))
              (setf openList (cdr openList))
              (incf heuristicCount)
            )
          )
        )
      )
    )
  )
)

#|
	Author: Jason Anderson
	Description: depending on chosen heuristic this performs the appropriate action
	Arguments: heuristic - which heuristic is being used
			   curNode - the current node in the state search
			   goalState - the desired state for a goal
			   n - size of puzzle
			   depth - how far into the solution path the curNode is.  mepth=g(n)
|#
(defun chooseHeuristic (heuristic curNode goalState n depth)
  (when (equal heuristic "Hamming")
    (return-from chooseHeuristic (+ depth (tileWrong curNode goalState n)))
  )
  (when (equal heuristic "Manhattan")
    (return-from chooseHeuristic (+ depth (Manhattan curNode n)))
  )
  (when (equal heuristic "Inadmissible")
    (return-from chooseHeuristic (+ depth (+ (tileWrong curNode goalState n) (Manhattan curNode n))))
  )
)

#|
	Author: Jason Anderson
	Description: using the Manhattan distance to determine how far a solution is from the goal state
	Arguments: curNode - the current node in the state search
			   n - square root of length of inList ex if inList is (1 2 3 4 5 6 7 8 0) then n=3
|#
(defun Manhattan (curNode n)
  (let (tempNode tNode totalTiles location)
    (setf goalStateLoc (generate_goalStateLoc n))
    (setf totalTiles 0)
    (setf tempNode (car curNode))
    (loop 
      for i from 0 to (- n 1) do
        (setf tNode (car tempNode))
        (loop
          for j from 0 to (- n 1) do
            (setf location (nth (car tNode) goalStateLoc))
            (setf totalTiles (+ totalTiles (abs (- (car location) i))))
            (setf totalTiles (+ totalTiles (abs (- (cadr location) j))))
            (setf tNode (cdr tNode))
        )
        (setf tempNode (cdr tempNode))
    )
    (return-from Manhattan totalTiles)
  )
)

#|
	Author: Jason Anderson
	Description: checks how many spots are wrong compared to the given goal state
	Arguments: curNode - the current state on the search for the solution
			   goalState - the solution for the given puzzle size
			   n - square root of length of inList ex if inList is (1 2 3 4 5 6 7 8 0) then n=3
|#
(defun tileWrong (curNode goalState n)
  (let (numWrong tempNode tempState tNode tState)
    (setf numWrong 0)
    (setf tempNode (car curNode))
    (setf tempState goalState)
    (loop
      for i from 1 to n do
        (setf tNode (car tempNode))
        (setf tState (car tempState))
        (loop
          for j from 1 to n do
            (if (not (equal (car tNode) (car tState))) (incf numWrong))
            (setf tNode (cdr tNode))
            (setf tState (cdr tState))
        )
        (setf tempNode (cdr tempNode))
        (setf tempState (cdr tempState))
      )
    (return-from tileWrong numWrong)
  )
)

