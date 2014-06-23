(define apply-in-underlying-scheme apply)


(define (eval exp env)
    ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating  exp))
        ((variable? exp) (analyze-variable exp))
        ((quoted? exp) (analyze-quoted exp))
          ; No Mutation ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((and? exp) (analyze-and exp))
        ((or? exp) (analyze-or exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
	(else (error "Unknown expression type -- ANALYZE" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	   (procedure-body procedure)
	   (extend-environment
	     (procedure-parameters procedure)
	     arguments
	     (procedure-environment procedure))))
	(else
	 (error "Unknown procedure type -- APPLY" procedure))))



(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
	(error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
		    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))
(define (analyze-self-evaluating exp) (lambda (env) exp))

(define (variable? exp) (symbol? exp))
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (quoted? exp) 
  (tagged-list? exp 'quote))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (caddr exp))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))



;; eval-and takes list of expressions (not including 'and) and evaluates left
;; to right. If any exp evaluates to false, false is returned. If all evaluate
;; to true, then the value of last expression is returned. 
(define (and? exp) (tagged-list? exp 'and))

(define (analyze-and exp)
  (let ((conjs (map analyze (cdr exp))))
    (define (eval-and conjs env last)
      (if (null? conjs) last
	  (let ((first ((car conjs) env)))
	    (if (true? first)
		(eval-and (cdr conjs) env first)
		false))))
      (lambda (env) (eval-and conjs env true))))



;; ((eval-or (cdr exp) env)))

;; eval-or takes list of expressions (not including 'or) and evalutes left
;; to right. If any expression evalutes to true, that value is returned.
;; if all expressions are false, then false is returned.
(define (or? exp) (tagged-list? exp 'or))
(define (analyze-or exp) 
  (let ((disjs (map analyze (cdr exp))))
    (define (eval-or disjs env)
      (if (null? disjs) false
	  (let ((first ((car disjs) env)))
	    (if (true? first) first
	    (eval-or (cdr disjs) env)))))
    (lambda (env) (eval-or disjs env))))

     
		
	    

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
			   (map (lambda (aproc) (aproc env))
				aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment (procedure-parameters proc)
			      args
			      (procedure-environment proc))))
	(else
	 (error
	  "Unkown procedure type -- EXECUTE-APPLICATION"
	  proc))))


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-vars bindings) (map car bindings))
(define (let-exps bindings) (map cadr bindings))
(define (let-body exp) (caddr exp))

(define (let->combination exp)
    (let ((vars (let-vars (let-bindings exp)))
          (exps (let-exps (let-bindings exp)))
          (body (let-body exp)))
         (if (not (= (length vars) (length exps)))
             (error "Ill-formed special form LET")
             (cons (list 'lambda vars body) exps))))
             

;; testing of predicates
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))


;; representing procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;; operations on environments

;; environments are lists: the enclosing env. is the cdr of the list.
;; the empty environment is the empty list.
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing environment env)))
	    ((eq? var (car vars))
	     (set-car! (vals val))
	    (else (scan (cdr vars) (cdr vals))))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))



(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))



(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list '+ +)
          (list '- -)
          (list '* *)
          (list '= =)
          (list 'list list)
          (list 'eq? eq?)
          (list 'null? null?)))


(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;; need a REPL here
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
 (prompt-for-input input-prompt)
 (let ((input (read)))
   (let ((output (eval input the-global-environment)))
     (announce-output output-prompt)
     (user-print output)))
 (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))


;;
;; This marks the end of the scheme interpreter as described in SICP
;; What follows is the transformation described in 
;;  "lightweight implementations of probabilistic programming languages..."
;; 
;; By "address" we mean a list of unique function-identifying symbols that
;; is constructed at runtime. 


;; define a syntactic transformation on expressions such that 
;;   1) each function definition takes another argument, addr
;;   2) change each function application such that
;;        * the current address is extended with a symbol that uniquely 
;;          identifies the application within the program
;;        * we pass on the modified address as an argument when the
;;          function is applied


;; primitive procedures need to be redefined to take (and ignore) the address

(define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list '+ +)
          (list '- -)
          (list '* *)
          (list '= =)
          (list 'list list)
          (list 'eq? eq?)
          (list 'null? null?)))



;; generates a unique symbol (convert to and from string to simplify)
(define (gen-addr) 
  (string->symbol (symbol->string (generate-uninterned-symbol "a"))))

(define (transform-top exp) 
  (generate-uninterned-symbol 0)
  (list (append
	   '(lambda (addr))
	   (list (transform exp)))
	'(top)))
	
		
(define (transform exp) 
  (cond ((lambda? exp) (transform-lambda exp))
	((begin? exp) (transform-begin exp))
	((let? exp) (transform-let exp))
	((if? exp) (transform-if exp))
	((and? exp) (transform-and exp))
	((or? exp) (transform-or exp))
	((definition? exp) (transform-definition exp))
	((quoted? exp) exp)
	((application? exp) (transform-application exp))
	((cond? exp) (transform (cond->if exp)))
	;; if no match, treat as primitive
	(else exp)))

(define (transform-lambda exp)
  (display "transform-lambda ")
  (let ((args (lambda-parameters exp))
	(body (lambda-body exp)))
    (display body)
    (list 'lambda (cons 'addr args) (transform body))))

(define (transform-begin exp)
  (display "transform-begin ")
 (cons 'begin (map transform (begin-actions exp))))

(define (transform-let exp) 
  (display "transform-let ")
  (let* ((bindings (let-bindings exp))
	(body (let-body exp))
	(new-bindings (map
		       (lambda (x) (list (car x) (transform (cadr x))))
		       bindings))
	(new-body (transform body)))
    (list 'let new-bindings new-body)))



(define (transform-if exp)
  (display "transform-if ")
  (cons 'if (map transform (cdr exp))))

(define (transform-and exp)
  (display "transform-and ")
  (cons 'and (map transform (cdr exp))))

(define (transform-or exp) 
  (display "transform-or ")
  (cons 'or (map transform (cdr exp))))

(define (transform-definition exp) 
  (display "transform-definition ")
  (let ((texp (transform (definition-value exp))))
    (list 'define (definition-variable exp) texp)))

(define (transform-application exp) 
  (display "transform-app ")
  (let* ((S (gen-addr))
	 (op (operator exp))
	 (args (operands exp))
	 (top (transform op))
	 (targs (map transform args)))
    (append (list (transform op) (list 'cons S 'addr)) targs)))

(define (transform-loop)
 (prompt-for-input input-prompt)
 (let ((input (read)))
   (let ((output (transform-top input)))
     (announce-output output-prompt)
     (user-print output)))
 (transform-loop))
