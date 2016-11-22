(defun FWGC ()
	
    (let 
        ( 
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
        (t nil)                                     ;No items left on left bank.
    )        
)



(defun apply-rules (state goal)

    (cond 

        ((equalp (nth 0 state) (nth 1 state))

            (dfs (rule-farmer-takes-wolf state) goal)

        )
        
        ((equalp (nth 0 state) (nth 2 state)) 

            (dfs (rule-farmer-takes-goat state) goal)

        )


        ((equalp (nth 0 state) (nth 3 state)) 

            (dfs (rule-farmer-takes-cabbage state) goal))

        (t 

            (dfs (rule-farmer-takes-self state) goal)

        )   
    )
)




(defun rule-farmer-takes-wolf (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 1 state) (boat-ride (nth 1 state) ) )
    (print state)
    (if t state nil) 
)


(defun rule-farmer-takes-goat (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 2 state) (boat-ride (nth 2 state) ) ) 
    (print state)
    (if t state nil) 
)


(defun rule-farmer-takes-cabbage (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 3 state) (boat-ride (nth 3 state) ) ) 
    (print state)
    (if t state nil) 
)


(defun rule-farmer-takes-self (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )     
    (print state)
    (if t state nil) 
)
        

(defun boat-ride (coming-from)
    (cond 
        ((equalp coming-from 'l) 'r)
        (t 'l)
    )
)


(defun consequences (state)
    (cond
        ((equalp (nth 0 state) 'l)          ;Farmer on left bank
            
            (cond 
                
                ((equalp (nth 1 state) (nth 2 state) 'r)    ;Wolf, goat alone r
    
                    (setf (nth 2 state) nil )
                )   

                ((equalp (nth 2 state) (nth 3 state) 'r)    ;Goat cabb alone r
    
                    (setf (nth 3 state) nil )
                ) 
            )
        )

        ((equalp (nth 0 state) 'r)          ;Farmer on right bank
        
            (cond
                
                ((equalp (nth 1 state) (nth 2 state) 'l)    ;Wolf, goat alone l
    
                    (setf (nth 2 state) nil )
                )   

                ((equalp (nth 2 state) (nth 3 state) 'l)    ;Goat cabb alone l
    
                    (setf (nth 3 state) nil )
                ) 
            )
        )    
    )
    (if t state nil)
)


(FWGC)
