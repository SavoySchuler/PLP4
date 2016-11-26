#|
        ***** fwgc.lsp *****

A farmer with his wolf, goat, and cabbage arrive at the bank of a river. A boat 
at the river’s edge is only large enough for the farmer and one of his 
possessions. The farmer cannot leave the wolf alone with the goat, or the goat 
alone with the cabbage. How may the farmer cross the river with all of his 
possessions intact?

This program solves the Farmer, Wolf, Goat, Cabbage problem by using a recursive
depth-first search as a state space approach to finding a solution.

This program uses the function (fwgc) as a simulated main and has an out-of-
function (fwgc) call placed at the bottom of the file to allow the program to be
run without entering an interpreter and manually loading the file.

Author: Savoy Schuler
Class:  CSC461 Programming Languages Fall 2016
Date:   11-25-16

|#


(defun fwgc ()
    "(fwgc): returns t if program completes successfully, else nil

        This is the simulated program main. It sets up local variables for the
        initial state of the problem and passes the into a deprth-first search.
        If the search is successful, the dfs first move to an output function to 
        print the solution path, but will then return t up the recursive calls.
        If no solution is found, nil will be returned and the function will 
        output a message to the user before evaluating nil."

    ;Set up initial state of search.
    (let 
        ( 
            ( start '(l l l l) ) 
            ( last-taken  'f )
            ( path  nil )
            ( action "*start state*" )
        )   

        ;Begin dfs. 
        (cond
            ;If successful, 'output' will print solution path to terminal.
            ((dfs start last-taken path action) t)

            ;If unsuccessful, inform user with output.
            (t (format t "~%~%No solution found.~%~%") nil)	
        )	
    )
)


(defun dfs (state last-taken path action)
    "(dfs state last-taken path action): 
    
        The depth-first search function will first check if the goal '(f w g c) 
        has been reached, if so it will proceed to the output function. Else it 
        will check if the state is still searchable (no nils), if so it will 
        proceed to recursively search by calling 'apply-rules'. Last, it will 
        default to return nil to indicate a dead-end path."

    ;Check conditions of state to determine appropriate path for dfs.
    (cond 
        ;If goal state reached, proceed to output function to print path.       
        ((equal state '(r r r r)) (setf path (cons (nconc state (list action))
             path)) (output (reverse path))
        )
        
        ;If no element has been set to nil, continue depth-first search.
        ((not (member nil state)) (apply-rules state last-taken path action) )

        ;If an element has be set to nil, return nil.        
        (t nil)
    )        
)


(defun apply-rules (state last-taken path action)
    "(apply-rules state last-taken path action): returns success of rules

        Called from dfs to continue search by branching valid moves from the 
        current state. The 'last-taken' variable is maintained to prevent the 
        dfs from recursively doing the inverse of the move it made previously, 
        which would otherwise result in infinite recursion. 'path' is maintained 
        to keep track of the solution path. At each pass of this function the 
        action of the previous pass is appended to the path, it is done here for
        convenience and localizing path manipulation calls."

    ;Must save the current state for backing up from dead ends. 
    ;Achieve this by passing each branch a copy of the current state.  
    (let (
        ( state-copy1 (copy-list state))          
        ( state-copy2 (copy-list state))
        ( state-copy3 (copy-list state))
        ( state-copy4 (copy-list state))
        )

    ;Update the solution path with the current state and last action. 
    (setf path (cons (nconc state (list action)) path))

    ;Test each move for validity and success.
    (cond 
        
        ;Try moving the farmer alone.
        ((valid-move state-copy1 '0 '0 'f last-taken path "farmer returns alone")
                t
        )

        ;Try moving the farmer and the wolf.
        ((valid-move state-copy2 '0 '1 'w last-taken path "farmer takes wolf across")
                t
        )

        ;Try moving the farmer and the goat.
        ((valid-move state-copy3 '0 '2 'g last-taken path "farmer takes goat across")
                t
        )

        ;Try moving the farmer and the cabbage.
        ((valid-move state-copy4 '0 '3 'c last-taken path "farmer takes cabbage across")
                t
        )
    )
    )
)


(defun valid-move (state farmer object object-name last-taken path action)
    "(valid-move state farmer object object-name last-taken path action): t/nil
    
        This function will test the validity of a move by with three criteria. 
        First, it will check if the farmer and the object are on the same side, 
        and thus if the farmer is able to move the object. It will then check 
        that the object was not the last one moved to prevent infinite recursion
        that could occur by repeatedly performing the inverse of the previous 
        move. Last it will call the dfs on the the state to see if a solution
        path is produced. The dfs is used here is a conditional to return t when
        the dfs returns t. This prevents a dead-end path from returning nil all
        the way up to the main function."

    (cond 
        ((and             
            (and   

                ;Check that the farmer and the object are on the same side.                  
                (equalp (nth farmer state) (nth object state)) 
                
                ;Check that the current object was not also the last one take.
                ;Prevents infinite recursion caused by calling inverse of prev.
                (not (equalp last-taken object-name))
            )

        ;Make move and pass state to consequences.
        (consequences (make-move state farmer object) object-name path action)
        )
                ;Return true if all three conditions pass => solution found.
                t
        )
    )
)


(defun make-move (state farmer object)
    "(make-move state farmer object):   returns new state with farmer and object
                                        moved

        This function is used to *make a move* by changing the position of the 
        farmer and the passed in object from one bank to the other. If the 
        farmer is traveling alone, the farmer's location '0 will be passed in 
        for both the farmer and the object."

    ;Move the farmer from one bank to the other.
    (setf (nth farmer state) (cond ((equalp (nth farmer state) 'l) 'r) (t 'l) ))  
    
    ;Move the object if one was passed. 'object' equals '0 if only farmer.
    (cond ((not (equalp farmer object)) (setf (nth object state) 
        (cond ((equalp (nth object state) 'l) 'r) (t 'l) )))
    )

    ;Trick to evaluate and return the whole state. Else returns element moved.     
    (if t state nil)  
)


(defun consequences (state last-taken path action)
   "(consequences state last-taken path action): returns will call to dfs

        This function evaluates the consequences of applying a rule to a state. 
        If the wolf and the goat are left alone, the wolf will eat the goat 
        and thus the goat's location in the state will be replaced by nil. If 
        the goat and the cabbage are left alone, the goat will eat the 
        cabbage and thus the cabbage's location in the state will be replaced by
        nil. These affects occur only when the farmer is on the opposite side. 
        The function will pass the potentially altered state to dfs for 
        evaluation."
 
    ;Check which bank farmer is on. 
    (cond
 
       ;Farmer on left bank.
        ((equalp (nth 0 state) 'l)          

            (cond 
                
                ;Wolf and goat are on same bank, wolf on right bank. Eat goat.
                ((and (equalp (nth 1 state) (nth 2 state)) (equalp (nth 1 state) 'r))  
                    (setf (nth 2 state) nil )
                )   

                ;Goat and cabbage are on same bank, goat on right bank. Eat cab.
                ((and (equalp (nth 2 state) (nth 3 state)) (equalp (nth 2 state) 'r))     
                    (setf (nth 3 state) nil )
                )
            )
        )        
    
        ;Farmer on right bank.
        ((equalp (nth 0 state) 'r)          

            (cond

                ;Wolf and goat are on same bank, wolf on left bank. Eat goat.                
                ((and (equalp (nth 1 state) (nth 2 state)) (equalp (nth 1 state) 'l))   
                    (setf (nth 2 state) nil )
                )   

                ;Goat and cabbage are on same bank, goat on left bank. Eat cab. 
                ((and (equalp (nth 2 state) (nth 3 state)) (equalp (nth 2 state) 'l))   
                    (setf (nth 3 state) nil )
                )
            )
        )
    )  

    ;Call dfs for evaluation.
    (dfs state last-taken path action)
)


(defun output (path)
        "(output path): returns t

        This function will convert states of a path from left/right notation to 
        left bank/right bank notation. It will then format and print each state 
        in the solution path with the action taken to produce the state. It will
        print a statement of successful completion of the program to the user 
        and return t."

    ;Print header.
    (format t "~%Left Bank       Right Bank      Action~%")
    (format t "---------       ----------      ------")

    ;For each state in solution path, convert to locational representation
    ;and display state and action taken.    
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

            ;Print left bank, right bank, and action of state to terminal.
            (format t "~%~9a~c~9a~c~a" (if (null left) "-" (reverse left)) #\Tab (if (null right) "-" (reverse right)) #\Tab (nth 4 state))
        )
    ) 

    ;Close with success. 
    (format t "~%~c~c~c~c*** problem solved! ***~%~%" #\Tab #\Tab #\Tab #\Tab) 
    t   
)


;Call to simulated main. Allows program to be run without entering interpreter.
(fwgc)
