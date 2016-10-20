
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
  (make-STATE :POS '(3 16)
	      :VEL '(1 3)
	      :ACTION act
	      :COST -100))


