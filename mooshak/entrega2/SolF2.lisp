;(load "datastructures.lisp")
;(load "auxfuncs.lisp")
;

(load "datastructures.fas")
(load "auxfuncs.fas")



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
(defun generateStates (st allActions stList)
    (if (null allActions)
        stList
        (generateStates st (rest allActions) (cons (nextState st (first allActions)) stList))
    )
)

(defun nextStates (st)
  "generate all possible next states"
	(generateStates st (possible-actions) '())
)



(defun limDFS (noodle prob limit sol)
    (if (funcall (problem-fn-isGoal prob) (node-state noodle))
        sol
        (if (= 0 limit)
            :corte
            (let ((result nil)
                 (newNoodle (copy-node noodle))
                 (cutoff nil))
                (loop for st in (funcall (problem-fn-nextStates prob) (node-state noodle)) do
                      
                        (setf (node-state newNoodle) st)
                        (setf result (limDFS newNoodle prob (1- limit) (append sol (list st))))
                        
                        (if (eql result :corte) (setf cutoff T)
                            (if result (return-from limDFS result))
                        )       
                )      
                        
                (if cutoff (return-from limDFS :corte)
                    (return-from limDFS NIL))
            )
        )
    )
)

;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim)
  #| limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit |#
	 (limDFS (make-node :state (problem-initial-state problem)) problem lim (list (problem-initial-state problem))) 
)
				      

;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem)
  #| limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations |#
    (let ((result nil)
          (lim most-positive-fixnum))
        (loop for i from 0 to lim do
            (setf result (limdepthfirstsearch problem i))
            (if (not (or (eq result :corte) (null result)))
                (return-from iterlimdepthfirstsearch result))  
        )
    )
    NIL
)