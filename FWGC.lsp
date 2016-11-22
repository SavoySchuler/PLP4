(defun FWGC ()
	
    (let ( 
            ( start '(l l l l) ) 
            ( goal  '(r r r r) ) 
         )   
    
		
		
    	(if (dfs start goal)			
			
    		(format t "Returned to main: solution found.~%~%")
    
    		(format t "Returned to main: no solution found.~%~%")	

	    )	

	)
)




(defun dfs (state goal)
	
    (format t "~%Inside DFS.~%")

    (cond 
        ((equal state goal) t)
        ((member nil state) nil)
        ((member 'l state)  (apply-rules state goal))
        (t nil)                                         ; No items left on left bank.
    )        
)



(defun apply-rules (state goal)
    (cond 
        ((equalp (nth 0 state) (nth 1 state)) (dfs (rule-farmer-takes-wolf state) goal))
;        ((equalp (nth 0 state) (nth 2 state)) (dfs (rule-farmer-takes-goat state) goal))
;        ((equalp (nth 0 state) (nth 3 state)) (dfs (rule-farmer-takes-cabbage state) goal))
;        (t (dfs (rule-farmer-takes-cabbage state) goal))   
    )
)




(defun rule-farmer-takes-wolf (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 1 state) (boat-ride (nth 1 state) ) ) 
)

(defun rule-farmer-takes-goat (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 2 state) (boat-ride (nth 2 state) ) ) 
)

(defun rule-farmer-takes-cabbage (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 3 state) (boat-ride (nth 3 state) ) ) 
)

(defun rule-farmer-takes-self (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )     
)

            

(defun boat-ride (coming-from)
    (cond 
        ((equalp coming-from 'l) 'r)
        (t 'l)
    )
)
   

(FWGC)
