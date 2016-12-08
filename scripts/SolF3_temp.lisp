(load "datastructures.lisp")
(load "auxfuncs.lisp")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
  (track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
   T)))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
    (make-vel (+ (vel-l (state-vel st)) (acce-l act))
        (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
    (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
        (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
    (cond ((isGoalp new-state) -100)
    ((isObstaclep (state-pos new-state) (state-track new-state)) 20)
    (T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
              (pos-c (state-pos st)))))
    (values new-state)))

;; Solution of phase 2

;;; Pedir 
(defun nextStates (st)
  "generate all possible next states"
  (let ((successors nil))
    (dolist (act (possible-actions) successors)
      (let ((new-state (nextState st act)))
  (if (not (member new-state successors :test #'equalp))
      (push new-state successors))))))

;;; Solucao e uma seq ordenada de estados
(defun solution (node)
  (let ((seq-states nil))
    (loop 
      (when (null node)
  (return))
      (push (node-state node) seq-states)
      (setf node (node-parent node)))
    (values seq-states)))

;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim &key cutoff?)
  #|limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit|#
  (labels ((limdepthfirstsearch-aux (node problem lim)
       (if (isGoalp (node-state node))
     (solution node)
     (if (zerop lim)
         :cutoff
         (let ((cutoff? nil))
           (dolist (new-state (nextStates (node-state node)))
       (let* ((new-node (make-node :parent node :state new-state))
        (res (limdepthfirstsearch-aux new-node problem (1- lim))))
         (if (eq res :cutoff)
             (setf cutoff? :cutoff)
             (if (not (null res))
           (return-from limdepthfirstsearch-aux res)))))
           (values cutoff?))))))
    (let ((res (limdepthfirstsearch-aux (make-node :parent nil :state (problem-initial-state problem))
          problem
          lim)))
      (if (eq res :cutoff)
    (if cutoff?
        :cutoff
        nil)
    res))))             

;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  #|limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations|#
  (let ((i 0))
    (loop
      (let ((res (limdepthfirstsearch problem i :cutoff? T)))
  (when (and res (not (eq res :cutoff)))
    (return res))
  (incf i)
  (if (> i lim)
      (return nil))))))
  
;; Solution of phase 3

;; Heuristic
(defun subPair (pair1 pair2)
  (list (- (first pair1) (first pair2)) (- (second pair1) (second pair2)))
)

(defun compute-heuristic (st)
 (let ( (track1 (state-track st)) (temp nil) (minDistance (list most-positive-fixnum  most-positive-fixnum)))
    (loop for end in (track-endpositions track1) do
      (setf temp (subPair end (state-pos st)))
      (if (< (first temp) (first minDistance))
          (setf (first minDistance) (first temp))
      )
      (if (< (second temp) (second minDistance))
        (setf (second minDistance) (second temp))
      )
    )
    (max (first minDistance) (second minDistance))
  )
)
  
(defun minNodeF (nodeList)
  (let ((nodeMinF (first nodeList)))
    (loop for n in nodeList do
      (if (< (node-f n) (node-f nodeMinF))
        (setf nodeMinF n)
      )
    )
    nodeMinF
  )
)

(defun buildPathAux (n path)
  (if (null n)
    path
    (buildPathAux (node-parent n) (append path (list (node-state n))))
  )
)


(defun buildPath (n)
  (buildPathAux n (list))
)
  
(defun stateMember (st nodeList);possivelmente nao é pa comparar tudo
  (let ((tempvar NIL))
    (loop for n in nodeList do
      (if (and (equal (state-pos st) (state-pos (node-state n)))
               (equal (state-vel st) (state-vel (node-state n)))
               ;(equal (state-action st) (state-action (node-state n)))
               ;(equal (state-cost st) (state-cost (node-state n)))
               ;(equal (state-track st) (state-track (node-state n)))
               ;(equal (state-other st) (state-other (node-state n)))        
          )
        (setf tempvar n)
      )
    )
    tempvar
  )
)


;;; A*

(defun a* (problem)
  (let ((closedSet (list))
        (openSet (list (make-node :state (problem-initial-state problem)
                            :parent NIL
                            :f (funcall (problem-fn-h problem) (problem-initial-state problem)) ;optimizar as duas chamadas?
                            :g 0
                            :h (funcall (problem-fn-h problem) (problem-initial-state problem)))))
        (current NIL)
        (neighborsOfCurrent (list))
        (temp_node NIL)
        (temp_gScore 0) ; maybe shouldn't start at 0
        )
    (loop while openSet do
      (setf current (minNodeF openSet))
      (if (funcall (problem-fn-isGoal problem) (node-state current))
        (return (buildPath current))
      )

      (setf openSet (remove current openSet))
      (setf closedSet (cons current closedSet))

      (setf neighborsOfCurrent (funcall (problem-fn-nextStates problem) (node-state current)))

      (loop for neighbor in neighborsOfCurrent do 
        (block continue
          (setf temp_node (stateMember neighbor closedSet))
          (if (null temp_node)  
            (return-from continue)
          )

          (setf temp_gScore (+ (node-g temp_node) (state-cost (node-state temp_node))))

          (if (not (stateMember neighbor openSet))
            (setf openSet (cons temp_node openSet))
            (if (>= temp_gScore (node-g temp_node))
              (return-from continue)
            )
          )

          (setf (node-g temp_node) temp_gScore)
          (setf (node-f temp_node) (+ (node-g temp_node) (funcall (problem-fn-h problem) (node-state temp_node))))

        )
      )
    )
  )
)
#|
;transcrito da wikipedia com mismatched parentesis somewhere





















(defun a* (problem);;replace compute-heuristic with funcal porblem-fn-h
  (let ( (closedSet (list))  ;(cameFrom (list)) 
         (openSet (list (make-node :state (problem-initial-state problem)
                                   :g 0
                                   :h (compute-heuristic (problem-initial-state problem))
                                   :f (compute-heuristic (problem-initial-state problem)))))
         (currentNode nil) (tempG nil) (tempNode nil))    
    (loop while openSet do
      (setf currentNode (minNodeF openSet))
      (if (member (state-pos (node-state currentNode))  
                  (track-endpositions (state-track (node-state currentNode)))  )
          (return (buildPath currentNode));;usa-se return?
      )
      (remove currentNode openSet)
      (cons currentNode closedSet)
      (loop for st in (nextStates (node-state currentNode)) do
        (if (not (stateMember st closedSet))
          (progn (setf tempG (+ (node-g currentNode) (state-cost st) ))
          (if (not (stateMember st openSet))
            (setf openSet 
              (cons (setf tempNode 
                          (make-node :state st 
                                     :parent currentNode
                                     :h (compute-heuristic (problem-initial-state problem))
                                     :g most-positive-fixnum))
              openSet))
          )
          (if (< tempG (node-g (stateMember st openSet)))
            (progn ;;(setf cameFrom (cons currentNode cameFrom))
            (remove tempNode openSet)
              (setf (node-g tempNode) tempG)
            (setf (node-f tempNode) (+ tempG (node-h tempNode)))
            (setf openSet (cons tempNode openSet)))                    
          )
        ))
      )     
    )
  )
)

|#