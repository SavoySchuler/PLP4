(defun FWGC ()
	
    (let 
        ( 
            ( start '(l l l l) ) 
            ( goal  '(r r r r) ) 
            ( last-taken  'f )
            ( path  nil )
        )   

        (cond
            ((dfs start goal last-taken path) t)
            (t (format t "~%~%Returned to main: no solution found.~%~%") nil)	
        )	
    )
)


(defun output (path)
    (format t "~%Left Bank    Right Bank    Action~%")
    (format t "---------    ----------    ------")
    (print path)
    t
)




(defun dfs (state goal last-taken path)
    (cond 
        ((equal state goal) (setf path(cons state path)) (output (reverse path)))
        ((not (member nil state)) (apply-rules state goal last-taken path) )
        (t nil)
    )        
)



(defun apply-rules (state goal last-taken path)
    (let (
        ( state-copy1 (copy-list state))          
        ( state-copy2 (copy-list state))
        ( state-copy3 (copy-list state))
        ( state-copy4 (copy-list state))
        )

    (setf path (cons state path))

    (cond 

        ((and 
            (not (equalp last-taken 'f))
            (consequences (rule-farmer-takes-self state-copy1) goal 'f path)
              
         )
            t
        )

        ((and (and 
        
            (equalp (nth 0 state) (nth 1 state)) 
            (not (equalp last-taken 'w))
            (consequences (rule-farmer-takes-wolf state-copy2) goal 'w path)
              )
         )
            t
        )
        

        ((and (and 
        
            (equalp (nth 0 state) (nth 2 state)) 
            (not (equalp last-taken 'g))
            (consequences (rule-farmer-takes-goat state-copy3) goal 'g path)
              )
         )
            t
        )


        ((and (and 
        
            (equalp (nth 0 state) (nth 3 state)) 
            (not (equalp last-taken 'c))
            (consequences (rule-farmer-takes-cabbage state-copy4) goal 'c path)
              )
         )
            t
        )
    )
    )
)


(defun rule-farmer-takes-self (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )     
    (if t state nil) 
)


(defun rule-farmer-takes-wolf (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 1 state) (boat-ride (nth 1 state) ) )
    (if t state nil)  
)


(defun rule-farmer-takes-goat (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 2 state) (boat-ride (nth 2 state) ) ) 
    (if t state nil) 
)


(defun rule-farmer-takes-cabbage (state)
    (setf (nth 0 state) (boat-ride (nth 0 state) ) )    
    (setf (nth 3 state) (boat-ride (nth 3 state) ) ) 
    (if t state nil) 
)
        

(defun boat-ride (coming-from)
    (cond 
        ((equalp coming-from 'l) 'r)
        (t 'l)
    )
)


(defun consequences (state goal last-taken path)
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
    (dfs state goal last-taken path)
)


(FWGC)
