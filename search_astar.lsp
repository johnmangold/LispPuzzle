(defstruct node state stateZloc stateDepth (heuristic 0 :type integer) parent)

;-------------------------------------------------------------------

(load 'generate_successorsN)

;-------------------------------------------------------------------

(defun search_Astar (inList zeroLoc n)
  (let (goalState (generate_goal_stateN n))
    (setf goalState (generate_goal_stateN n))
    (loop
      (let* ((curNode (make-node :state inList :stateZloc zeroLoc :stateDepth 0 :heuristic 0 :parent nil))
        (OPEN (list curNode))(CLOSED nil))

        (loop
          ;get current node from OPEN, update OPEN and CLOSED
          (setf curNode (car OPEN))
          (setf OPEN (cdr OPEN))
          (setf CLOSED (cons curNode CLOSED))

          ;check goal state
          (when (equal (node-state curNode) goalState)
                (print_solution_path curNode CLOSED)
                  (return-from search_Astar t)
          )

          ;generate successors
          (dolist (child (generate_successorsN (node-state curNode)(node-stateZloc curNode) n))
            ;for each child node
            (setf child (make-node :state (first child) 
                                   :stateZloc (second child)
                                   :stateDepth (1+ (node-stateDepth curNode))
                                   :heuristic (+ (node-stateDepth curNode) (tileWrong child goalState n))
                                   :parent (node-state curNode)))
            ;check if already on OPEN or CLOSED
            (when (and (not (member child OPEN :test #'equal-states))
                       (not (member child CLOSED :test #'equal-states)))
              ;put into open if not already in OPEN or CLOSED
              (setf OPEN (cons child OPEN))
            )    
          )
          ;put the lowest heuristic first
          (let (lowest heuristicCount openList)
(format t "in let~%")
            (setf heuristicCount 0)
            (setf openList OPEN)
            (dolist (car openList)
(format t "lowest: ~S node-heuristic: ~S~%" lowest (node-heuristic (car openList)))
              (cond 
                ((equal heuristicCount 0) (setf lowest (node-heuristic (car openList))) (format t "equal~%"))
                ((< (node-heuristic (car openList)) lowest) (setf lowest (node-heuristic (car openList))) (setf OPEN (cons (car openList) OPEN)) (format t "<~%"))
              )

(format t "complete car openlist: ~S~%" openList)
(format t "heuristic?: ~S~%" (node-heuristic (car openList)))
(format t "(1,1): ~S~%" (car (cdr (car (cdr (node-state (car openList)))))))
              (setf openList (cdr openList))
              (incf heuristicCount)
            )
          )
(format t "complete car OPEN: ~S~%" (car OPEN))
(format t "complete cdr OPEN: ~S~%" (car (cdr OPEN)))
(format t "wut~%")
(read)
        )
)
 )))

(defun tileWrong (curNode goalState n)
  ;(format t "curNode: ~S~%" curNode)
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
  (format t "numWrong: ~S~%" numWrong)
  (return-from tileWrong numWrong)
)

