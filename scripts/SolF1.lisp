
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
(load "datastructures.fas")
(load "auxfuncs.fas")

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
  	(let (newState)
  		(setf newState (make-State))
  		(setf (STATE-VEL newState) (addListN (STATE-VEL st) act))
  		(setf (STATE-POS newState) (addListN (STATE-POS st) (STATE-VEL newState)))
  		(setf (STATE-ACTION newState) act)
  		(setf (STATE-TRACK newState) (STATE-TRACK st))  		
  		(setf (STATE-COST newState)
  			(cond ((isObstaclep (STATE-POS newState) (STATE-TRACK newState)) 20)
  				  ((isGoalp newState) -100)
  				  (T 1)
  			)
  		)	
  		(setf (STATE-OTHER newState) (STATE-OTHER st))
  		newState
  	)
 )
