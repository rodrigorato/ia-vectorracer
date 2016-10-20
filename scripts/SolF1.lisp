
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
;(load "datastructures.fas")
;(load "auxfuncs.fas")

(defun isObstaclep (pos track) 
  (not (nth (second pos) (nth (first pos) (track-env track) ) ) )
)

(defun isGoalp (st) 
  (if (find (state-pos st) (track-endpositions (state-track st)) :test #'equal)
 	 t)
)

(defun addListN (l1 l2)
	(if (= (length l1) (length l2))
		(if (= 0 (length l1))
			'()
			(cons (+ (first l1) (first l2)) (addListN (rest l1) (rest l2)))
		)
		NIL
	)
)

(defun nextState (st act)
  	"generate the nextState after state st and action act"
  	(let (newState stVel)
  		(setf stVel (addListN (state-vel st) act))

  		(setf newState (make-STATE :VEL stVel
	    						   :POS (addListN (state-pos st) stVel)
	      						   :ACTION act
	      						   :TRACK (state-track st)))

	    (setf (state-cost newState) 
	   		(cond ((isGoalp newState) -100)
	     		  ((isObstaclep (state-pos newState) (state-track newState)) 20)
	     	      (T 1)
	     	)
	    )
	    newState
	)
)