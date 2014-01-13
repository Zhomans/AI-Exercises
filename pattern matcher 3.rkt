;Zachary Homans
;2/12/13
;Pattern Matcher!

;Message List. The first line of each sublist contains messages to be matched to user messages. The second line contains possible responses. * is a multiple word replacement function.


(define messages '(((((* x) NASCAR (* y)) ((* x) NASCAR) (NASCAR (* x)))
                    ((I do not actually watch NASCAR. Sorry.) (NASCAR is for chumps!)))
                   ((((* x) freedom (* y)) ((* x) liberty (* y)))
                    ((America is the single freest and libertious country in the whole world!) (Freedom and liberty are the best parts of ol America!)))
                   (((i like (* x)) (I like (* x))) 
                    ((Well if America likes x then I like it too.) (Tell America why you like x so much.)))
                   (((i dont like (* x)) (i do not like (* x)) (I dont like (* x)) (I do not like (* x)) (i hate (* x)) (I hate (* x))) 
                    ((You better watch what you say. People who dont like x arent too favored in America!)))
                   ((((* x) is my favorite person)) 
                    ((America could use some more people like x.) (Lady Liberty is my favorite person!)))
                   (((i am (* x)) (I am (* x)) (Im (* x)) (im (* x)))
                    ((For Americas sake, Im glad you are x) (If you werent x Id be disappointed in you son)))
                   (((my name is (* x)) (My name is (* x)))
                    ((I already told you my name x so dont make me repeat it.) (Thats nice I guess but what does it have to do with America!)))
                   (((America (* x)) ((* x) America (* y)) ((* x) America))
                     ((Ah good now were talking about the thing I actually care about!) (Ah America. Nothin beats it.)))
                   (((Canada (* x)) ((* x) Canada (* y)) ((* x) Canada))
                    ((Canada? You mean those hockey-lovers up on our heads?) (Canadas got nothin on good ol America!)))
                   ((((* x) watch?) ((* x) watch (* y)) ((* x) watch))
                    ((The only thing I watch is good ol Fox News.)))
                   
;Obviously more message and response sets could be added very easily.                                     
                   
                   ))

;My code
;;--------------------------------------------------------------------------------

;Base Function. Displays initial message.
(define pattern-matcher
  (lambda ()
    (begin
      (display "(Welcome to America-bot 5000 Extreme Turbo Deluxe Mk. V NASCAR Edition. How are you today fine sir? (No punctuation please!))\n")
      (pattern-matcher-help))))

;Cycles through the user messages and prints out appropriate response.
(define pattern-matcher-help
  (lambda ()
    (begin
      (display (check-messages (read) messages))
      (display "\n")
      (pattern-matcher-help))
      ))

;Checks a user message against the list of messages seen at the top of this file.
(define check-messages
  (lambda (message mlist)
    (cond
      ((null? mlist) (random-element (list '(Son I dont even know what to say to that.) '(Talk more about America!)))) ;If there's no matching pattern, it returns this.
      ((check-messages-help message (car (car mlist)) (cadr (car mlist))))
      (else (check-messages message (cdr mlist))))))
      
;Checks a user message against a set of message patterns that all correspond to the same responses.
(define check-messages-help
  (lambda (message mset rset)
    (cond
      ((null? mset) #f) ;Returns false if there is no matching pattern in the set.
      ((formulate-message (random-element rset) (check-var (check-message message (car mset) '())))) ;If there is a matching pattern, formulate and evalute the appropriate response.
      (else (check-messages-help message (cdr mset) rset)))))

;Checks a user message against an individual message pattern.
(define check-message
  (lambda (my-message tbc-message var-list)
    (cond
      ((and (null? my-message) (null? tbc-message)) (if (null? var-list)
                                                        #t
                                                        var-list)) ;If both messages end at the same time and there hasn't been a conflict, return either true or the list of variable associations.
      ((or (null? my-message) (null? tbc-message)) #f) ;If the two don't end at the same time, they can't be the same.
      ((list? (car tbc-message)) (cond ;If there is a special form:
                                   ((equal? (car (car tbc-message)) '=) (check-message (cdr my-message) (cdr tbc-message) (cons (list (cadr (car tbc-message)) (car my-message)) var-list))) ;Takes care of single-word replacements.
                                   ((equal? (car (car tbc-message)) '*) (update-* my-message tbc-message var-list '())))) ;Takes care of multi-word replacements.
      ((equal? (car my-message) (car tbc-message)) (check-message (cdr my-message) (cdr tbc-message) var-list))
      (else #f)))) ;If there's no match, return false.

;Formulate a response given the appropriate output message and association list.
(define formulate-message
  (lambda (out-message var-list)
    (cond
      ((boolean? var-list) (if var-list
                               out-message  ;If it's true, there are no variables to be swapped.
                               #f)) ;If it's false, then there's no match!
      (else (output-message out-message var-list '()))))) ;Otherwise, actually go and create the response message.

;Handles multi-word replacement.
(define update-*
  (lambda (my-message tbc-message var-list new-var)
    (cond
      ((null? my-message) (check-message my-message (cdr tbc-message) (cons (cons (cadr (car tbc-message)) (list new-var)) var-list))) ;If the message is over, add the new variable association to the list and quit the multi-word replacement.
      ((null? (cdr tbc-message)) (update-* (cdr my-message) tbc-message var-list (append new-var (list (car my-message))))) ;If the last part of the pattern is a multi-word replacement, the rest of the message is in that replacement.
      ((equal? (car my-message) (cadr tbc-message)) (check-message my-message (cdr tbc-message) (cons (cons (cadr (car tbc-message)) (list new-var)) var-list))) ;If the next word in the pattern is the current word in the message, end the multi-word replacement.
      (else (update-* (cdr my-message) tbc-message var-list (append new-var (list (car my-message)))))))) ;Otherwise, just add what's there to the replacement.                                                                      

;Changes the output message based on the association list.
(define output-message
  (lambda (old-message var-list new-message)
    (cond
      ((null? old-message) (flatten new-message))
      (else (output-message (cdr old-message) var-list (append new-message (list (replace-word (car old-message) var-list))))))))

;Replaces individual words with the appropriate associated word.
(define replace-word
  (lambda (word dict)
    (cond
      ((null? dict) word)
      ((equal? word (car (car dict))) (cadr (car dict)))
      (else (replace-word word (cdr dict))))))   

;Makes sure that the same variable isn't defined twice differently. Outputs false if they are. Otherwise, outputs the association list.
(define check-var
  (lambda (var-list)
    (cond
      ((boolean? var-list) var-list)
      (else (check-var-help (car var-list) (cdr var-list) var-list var-list)))))

(define check-var-help
  (lambda (curr-var curr-var-list var-list perm-var-list)
    (cond
      ((null? var-list) perm-var-list)
      ((null? curr-var) (check-var-help (car var-list) (cdr var-list) var-list perm-var-list))
      ((null? curr-var-list) (check-var-help '() '() (cdr var-list) perm-var-list))
      ((equal? (car curr-var) (car (car curr-var-list))) (if (equal? (cadr curr-var) (cadr (car curr-var-list)))
                                                             (check-var-help curr-var (cdr curr-var-list) var-list perm-var-list)
                                                             #f))
      (else (check-var-help curr-var (cdr curr-var-list) var-list perm-var-list)))))


;Code I didn't write myself
;;--------------------------------------------------------------
(define flatten
  (lambda (lst)
    (if (null? lst)
        lst
        (append
          (rtn-lst (car lst))
          (flatten (cdr lst))))))

(define rtn-lst
  (lambda (lst)
    (cond 
      ((list? lst)
        (if (null? lst)
            empty
            (flatten lst)))
      (else 
        (list lst)))))

(define (random-element lst)
  (list-ref lst (random (length lst))))

;;----------------------------------------------------------------

(pattern-matcher)

;(Welcome to America-bot 5000 Extreme Turbo Deluxe Mk. V NASCAR Edition. How are you today fine sir? (No punctuation please!))
;(america)
;(Son I dont even know what to say to that.)
;(america)
;(Talk more about America!)
;(america)
;(Talk more about America!)
;(i live in america)
;(Talk more about America!)
;(America)
;(Ah good now were talking about the thing I actually care about!)
;(yes)
;(Talk more about America!)
;(America)
;(Ah good now were talking about the thing I actually care about!)
;(America is cool)
;(Ah America. Nothin beats it.)
;(I am Asian American)
;(If you werent Asian American Id be disappointed in you son)
;(I like to eat america)
;(Tell America why you like to eat america so much.)
;(america, you are delicious)
;(Talk more about America!)
;(America, you are delicious)
;(Ah good now were talking about the thing I actually care about!)
;(yes)
;(Talk more about America!)