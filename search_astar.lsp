;-------------------------------------------------------------------

(load 'generate_successorsN)
(load 'goalStateLoc)

;-------------------------------------------------------------------

(defun search_Astar (inList zeroLoc n heuristic)
  (let ((goalState (generate_goal_stateN n))
       (solutionPath)
       (nodesGenerated 0)
       (nodesPlacedOpen 0)
       (nodesPlacedClosed 0))
    (setf goalState (generate_goal_stateN n))
    (loop
      (let* ((curNode (make-node :state inList :stateZloc zeroLoc :stateDepth 0 :heuristic 0 :parent nil))
        (OPEN (list curNode))(CLOSED nil))

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
            (format t "~S nodes generated (~S 'distinct' nodes), ~S nodes expanded ~%"
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
                                   ;:heuristic (+ (node-stateDepth curNode) (tileWrong child goalState n))
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
          (let (lowest heuristicCount openList storeLowest)
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

(defun chooseHeuristic (heuristic curNode goalState n depth)
  (when (equal heuristic "Hamming")
    (return-from chooseHeuristic (+ depth (tileWrong curNode goalState n)))
  )
  (when (equal heuristic "Manhattan")
    (return-from chooseHeuristic (Manhattan curNode n))
  )
)

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

