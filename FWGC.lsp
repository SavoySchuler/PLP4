(defun FWGC ()
	
    (let 
        ( 
            ( start '(l l l l) ) 
            ( goal  '(r r r r) ) 
        )   
	
        (if (dfs start goal 'f)			
            (format t "~%~%Returned to main: solution found.~%~%")
            (format t "~%~%Returned to main: no solution found.~%~%")	
        )	
    )
)




(defun dfs (state goal last-taken)
	

    (format t "~%~%Inside DFS. State:")
    (print state)

    (cond 
        ((equal state goal) t)
        ((and (not (member nil state)) (member 'l state))  
            
            (apply-rules state goal last-taken)
        
        )

       ; (t nil)                                     ;No items left on left bank.
    )        
)



(defun apply-rules (state goal last-taken)

        (format t "~%~%In applying rules.")

    (setf state-copy1 (copy-list state))          
    (setf state-copy2 (copy-list state))
    (setf state-copy3 (copy-list state))
    (setf state-copy4 (copy-list state))

    (cond 

        ((and 
            (not (equalp last-taken 'f))
            (consequences (rule-farmer-takes-self state-copy1) goal 'f)
              
         )
            t
        )

        ((and (and 
        
            (equalp (nth 0 state) (nth 1 state)) 
            (not (equalp last-taken 'p))
            (consequences (rule-farmer-takes-wolf state-copy1) goal 'w)
              )
         )
            t
        )
        

        ((and (and 
        
            (equalp (nth 0 state) (nth 2 state)) 
            (not (equalp last-taken 'g))
            (consequences (rule-farmer-takes-goat state-copy2) goal 'g)
              )
         )
            t
        )


        ((and (and 
        
            (equalp (nth 0 state) (nth 3 state)) 
            (not (equalp last-taken 'c))
            (consequences (rule-farmer-takes-cabbage state-copy3) goal 'c)
              )
         )
            t
        )


   
    )

    
)




(defun rule-farmer-takes-wolf (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 1 state) (boat-ride (nth 1 state) ) )
    
    (format t "~%~%Rule 1")
 

    (if t state nil)  
)


(defun rule-farmer-takes-goat (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 2 state) (boat-ride (nth 2 state) ) ) 
    
    (format t "~%~%Rule 2")


    (if t state nil) 
)


(defun rule-farmer-takes-cabbage (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 3 state) (boat-ride (nth 3 state) ) ) 
    
    (format t "~%~%Rule 3")


    (if t state nil) 
)


(defun rule-farmer-takes-self (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )     
    
    (format t "~%~%Rule 4")


    (if t state nil) 
)
        

(defun boat-ride (coming-from)
    (cond 
        ((equalp coming-from 'l) 'r)
        (t 'l)
    )
)


(defun consequences (state goal last-taken)
    
    (format t "~%~%Before Consequence:")
    (print state)    

    (cond
        ((equalp (nth 0 state) 'l)          ;Farmer on left bank
            
            (cond 
                
                ((and (equalp (nth 1 state) (nth 2 state)) (equalp (nth 1 state) 'r))    
                ;Wolf, goat alone r
    
                    (setf (nth 2 state) nil )
                )   

                ((and (equalp (nth 2 state) (nth 3 state)) (equalp (nth 2 state) 'r))    
                ;Goat cabb alone r
    
                    (setf (nth 3 state) nil )
                ) 
            )
        )

        ((equalp (nth 0 state) 'r)          ;Farmer on right bank
        
            (cond
                
                ((and (equalp (nth 1 state) (nth 2 state)) (equalp (nth 1 state) 'l))   
                ;Wolf, goat alone l
    
                    (setf (nth 2 state) nil )
                )   

                ((and (equalp (nth 2 state) (nth 3 state)) (equalp (nth 2 state) 'l))   
                ;Goat cabb alone l
    
                    (setf (nth 3 state) nil )
                ) 
            )
        )    
    )


        
    (format t "~%~%Applied Consequence:")
    (print state)

    (dfs state goal last-taken)
)



(FWGC)
