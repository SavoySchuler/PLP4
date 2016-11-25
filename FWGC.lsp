(defun FWGC ()
    (let 
        ( 
            ( start '(l l l l) ) 
            ( last-taken  'f )
            ( path  nil )
            (action "*start state*")
        )   

        (cond
            ((dfs start last-taken path action) t)
            (t (format t "~%~%Returned to main: no solution found.~%~%") nil)	
        )	
    )
)


(defun output (path)
    (format t "~%Left Bank       Right Bank      Action~%")
    (format t "---------       ----------      ------")
    (loop for state in path do 
        (let( 
            (left nil)
            (right nil)
            )
        (cond
            ((equalp (nth 0 state) 'l) (setf left '(f)))
            ((equalp (nth 0 state) 'r) (setf right '(f)))
        )    
        (cond
            ((equalp (nth 1 state) 'l) (setf left (cons 'w left)))
            ((equalp (nth 1 state) 'r) (setf right (cons 'w right)))
        )  
        (cond
            ((equalp (nth 2 state) 'l) (setf left (cons 'g left)))
            ((equalp (nth 2 state) 'r) (setf right (cons 'g right)))
        ) 
        (cond
            ((equalp (nth 3 state) 'l) (setf left (cons 'c left)))
            ((equalp (nth 3 state) 'r) (setf right (cons 'c right)))
        ) 
            (format t "~%~9a~c~9a~c~a" (if (null left) "-" (reverse left)) #\Tab (if (null right) "-" (reverse right)) #\Tab (nth 4 state))
        )
    ) 
    (format t "~%~c~c~c~c*** problem solved! ***~%~%" #\Tab #\Tab #\Tab #\Tab) 
    t    
)


(defun dfs (state last-taken path action)
    (cond 
        ((equal state '(r r r r)) (setf path (cons (nconc state (list action)) path)) (output (reverse path)))
        ((not (member nil state)) (apply-rules state last-taken path action) )
        (t nil)
    )        
)


(defun apply-rules (state last-taken path action)
    (let (
        ( state-copy1 (copy-list state))          
        ( state-copy2 (copy-list state))
        ( state-copy3 (copy-list state))
        ( state-copy4 (copy-list state))
        )

    (setf path (cons (nconc state (list action)) path))

    (cond 

        ((and 
            (not (equalp last-taken 'f))
            (consequences (make-move state-copy1 '0 nil) 'f path "farmer returns alone")
              
         )
            t
        )

        ((and (and 
        
            (equalp (nth 0 state) (nth 1 state)) 
            (not (equalp last-taken 'w))
            (consequences (make-move state-copy2 '0 '1) 'w path "farmer takes wolf across" )
              )
         )
            t
        )
        

        ((and (and 
        
            (equalp (nth 0 state) (nth 2 state)) 
            (not (equalp last-taken 'g))
            (consequences (make-move state-copy3 '0 '2) 'g path "farmer takes goat across")
              )
         )
            t
        )


        ((and (and 
        
            (equalp (nth 0 state) (nth 3 state)) 
            (not (equalp last-taken 'c))
            (consequences (make-move state-copy1 '0 '3) 'c path "farmer takes cabbage across")
              )
         )
            t
        )
    )
    )
)


(defun make-move (state farmer object)
    (setf (nth farmer state) (cond ((equalp (nth farmer state) 'l) 'r) (t 'l) ))  
    (cond ((not (null object)) (setf (nth object state) (cond ((equalp (nth object state) 'l) 'r) (t 'l) ))))  
    (if t state nil)  
)


(defun consequences (state last-taken path action)
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
    (dfs state last-taken path action)
)


(FWGC)
