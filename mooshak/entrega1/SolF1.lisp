; Grupo #60 -- Alice Dourado - 81205 -- Rodrigo Rato - 81500

;;; Includes para a versao a submeter
(load "datastructures.fas")
(load "auxfuncs.fas")

; Verifica se uma posicao e' um obstaculo numa pista
(defun isObstaclep (pos track) 
  (not (nth (second pos) (nth (first pos) (track-env track) ) ) )
)

; Verifica se um estado e' o estado objectivo
(defun isGoalp (st) 
  (if (find (state-pos st) (track-endpositions (state-track st)) :test #'equal)
 	 t)
)

; Devolve a lista em que cada elemento n e dado por l1[n] + l2[n]
; para listas de comprimentos iguais.
(defun addListN (l1 l2)
	(if (= (length l1) (length l2))
		(if (= 0 (length l1))
			'()
			(cons (+ (first l1) (first l2)) (addListN (rest l1) (rest l2)))
		)
		NIL
	)
)

; Gera o proximo estado a partir do estado st e a accao act
(defun nextState (st act)
  	(let (newState 
  		 (stVel (addListN (state-vel st) act)))

  		(setf newState (make-STATE :VEL stVel
	    						   :POS (addListN (state-pos st) stVel)
	      						   :ACTION act
	      						   :TRACK (state-track st)))

	    (setf (state-cost newState) 
	   		(cond ((isGoalp newState) -100)
	     		  ((isObstaclep (state-pos newState) (state-track newState)) 20)
	     	      (T 1)))

	    newState
	)
)