
;; global variables used for tracking log-probabilities
(define likelihood 0)
(define likelihood-fresh 0)
(define likelihood-stale 0)
(define current-trace 0) ; use to mark "active" random choices in the databae

(define apply-in-underlying-scheme apply)

(define (summarize-rnd) (begin 
    (display "\n")
    (display current-trace)
    (display "\n")
    (hash-table/for-each rnd-table (lambda (addr rnd)
	   (display-rnd-addr addr))) 
    (display "ll: ") (display likelihood)
    (display "\nll-fresh: ") (display likelihood-fresh)
    (display "\nll-stale:") (display likelihood-stale) (list) (display "\n")))


;; display the current type, value, parameters, likelihood
;; of the random choice associated with addr in the hash table
(define (display-rnd-addr addr)
  (let ((rnd (hash-table/get rnd-table addr 'nil)))
    (if (eq? rnd 'nil)
	(display "No value associated with address.\n")
	(begin (display addr)
	       (display ": ") (display (rnd-type rnd))
	       (display "  val:") (display (rnd-val rnd))
	       (display "  param:") (display (rnd-params rnd))
	       (display "  ll:") (display (rnd-ll rnd))
           (display " mark: ") (display (rnd-mark rnd))
	       (display "\n")
	       rnd))))


(define (set-trace-mark) 
    (set! current-trace
    (string->symbol (symbol->string (generate-uninterned-symbol "t"))))
    current-trace)

(define (get-current-trace) current-trace)



;; MCMC (Metropolis-Hastings) implementation

;; trace-update runs f, keeping track of likelihoods
;;   f: (transformed) probabilistic program that takes no parameters
;;   modifies rnd-table (global hash-table)
(define (trace-update f)
(begin
  (set-trace-mark)
  (set! likelihood 0)
  (set! likelihood-fresh 0)
  (set! likelihood-stale 0)

  (eval f the-global-environment) ; likelihood and random choices are tracked
  (hash-table/for-each rnd-table (lambda (addr rnd)
     (if (eq? (get-current-trace) (rnd-mark rnd))
	 (list)
	 (let* ((x (rnd-val rnd))
		(params (rnd-params rnd))
		(type (rnd-type rnd))
		(ll (get-ll type x params)))
	   (set! likelihood-stale (+ likelihood-stale ll))
	   (hash-table/remove! rnd-table addr)))))
  rnd-table))

(define (mh-loop exp iters)
    (if (eq? iters 0)
	(summarize-rnd)

	(let* ((addrs (hash-table/key-list rnd-table))
           (ll-old likelihood)
           ; choose a random name from the database, if empty, use the first choice.
           (name-r (list-ref addrs (random (length addrs))))
	       (choice-r (hash-table/get rnd-table name-r 'nil)) ; corresponding rnd 
	       (type-db (rnd-type choice-r))
	       (val-db (rnd-val choice-r)) 
	       (params-db (rnd-params choice-r))

	       ; sample from proposal disribution
	       (val-sample (get-kernel-sample type-db params-db val-db)) 
	       (F (get-kernel-prob type-db val-sample params-db))
	       (R (get-kernel-prob type-db val-db params-db))
	       (l (get-ll type-db val-sample params-db))

	       (old-table (rnd-table-cpy rnd-table))
	       (_ (hash-table/put! rnd-table name-r
		    (make-rnd type-db val-sample params-db l (get-current-trace))))
	       (table (trace-update exp))
	       (l- likelihood)
	       (l-fresh likelihood-fresh)
	       (l-stale likelihood-stale)
	       (alpha (- 
		       (+ likelihood R 
			  (log (hash-table/count old-table)) likelihood-stale) 
		       ll-old F
		       (log (hash-table/count rnd-table)) likelihood-fresh)))
        (if (< (log (flo:random-unit *random-state*)) alpha)
		    (set! likelihood l-)  ;; accept
            (set! rnd-table old-table)) ;; reject
        (mh-loop exp (- iters 1)))))



(define (mh-sample exp iters)
  (hash-table/clear! rnd-table)
  (set! rnd-table (trace-update exp))
  (mh-loop exp iters))
  	  
;; database of random choices
;; implemented as a hashtable from lists of symbols to rndm structures
;;
;; defines a mapping N -> T * X * L * 0, where
;;    N : name of random choice (a list of symbols)
;;    X : the value
;;    T : the ERP type
;;    0 : the ERP parameters
;;    L : likelihood


;; table mapping names to types, values, params, and likelihood
;; for now, a global variable. 
(define rnd-table (make-equal-hash-table))
;; rnd is a structure containing random data 
(define-structure (rnd copier) type val params ll mark)

;; returns deep copy of hash table and all structures in it
(define (rnd-table-cpy table)
  (let ((new-table (make-equal-hash-table)))
    (hash-table/for-each table (lambda (addr rnd)
	(hash-table/put! new-table addr (copy-rnd rnd)))) 
    new-table))



;;;;; implementations of random primitives go here ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bernoulli p) (if (flip p) 1 0))
(define (bernoulli-ll x p)
  (cond ((eq? x 1) (flip-ll true p)
	 (eq? x 0) (flip-ll false p)
	 (else (error "unsuppored value -- bernoulli-ll")))))
(define (bernoulli-kernel-sample theta . state)
  (apply-in-underlying-scheme bernoulli (list theta)))
(define (bernoulli-kernel-p x theta . state)
  (bernoulli-ll x theta))
(define (church-bernoulli addr p) (lookup-erp-value addr 'bernoulli p))


(define (flip p) (> p (flo:random-unit *random-state*)))
(define (flip-ll x p)
  (let ((k (if x 1 0)))
    (log (* (expt p k) (expt (- 1 p) (- 1 k))))))
(define (flip-kernel-sample theta . state)
      (apply-in-underlying-scheme flip (list theta)))
(define (flip-kernel-p x theta . state)
  (flip-ll x theta))
(define (church-flip addr p) (lookup-erp-value addr 'flip p))

; TODO: switch to inverse transform for generating
(define (geometric p) 
  (define (geometric-acc p k) (if (flip p) k (geometric-acc p (+ k 1))))
  (geometric-acc p 0))
(define (geometric-ll k p) (* p (expt (- 1 p) k)))
(define (geometric-kernel-sample p . state)
  (apply-in-underlying-scheme geometric (list p)))
(define (geometric-kernel-p k p . state)
  (geometric-ll k p))
(define (church-geometric addr p) (lookup-erp-value addr 'geometric p))



;; table maping erp types to underyling function
;; at the moment, you need to declare the erp name as a primitive procedure,
;; corresponding to a function that calls lookup-erp-value on the
;; appropriate type with some other function actually implementing the erp 
;; and register the implementation in erp-table
;; TODO: make this all more dynamic
(define erp-table (make-strong-eq-hash-table))
;; erp is a structure containing the erp implementation and likelihood
(define-structure erp sample ll kernel-sample kernel-p)
(hash-table/put! erp-table 'flip (make-erp flip flip-ll
					   flip-kernel-sample flip-kernel-p))
(hash-table/put! erp-table 'bernoulli (make-erp bernoulli bernoulli-ll
	    bernoulli-kernel-sample bernoulli-kernel-p))
(hash-table/put! erp-table 'geometric (make-erp geometric geometric-ll
	    geometric-kernel-sample geometric-kernel-p))

;; sample from P_type(*|params)
(define (get-sample type params)
  (let ((impl (erp-sample (hash-table/get erp-table type 'nil))))
    (apply-in-underlying-scheme impl params)))

;; compute the log-likelihood P_type(val|params)
(define (get-ll type val params)
  (let ((lkl (erp-ll (hash-table/get erp-table type 'nil))))
    (apply-in-underlying-scheme lkl (cons val params))))

;; sample from the proposal kernal 
(define (get-kernel-sample type params . state)
  (let ((kernel (erp-kernel-sample (hash-table/get erp-table type 'nil))))
    (apply-in-underlying-scheme kernel (append params state))))

;; evaluate proposal probability
(define (get-kernel-prob type val params . state)
  (let ((kernel (erp-kernel-p (hash-table/get erp-table type 'nil))))
    (apply-in-underlying-scheme kernel val params )))


(define (lookup-erp-value addr type . params)
 (let ((rnd (hash-table/get rnd-table addr 'nil)))
    (if (and (not (eq? rnd 'nil)) (eq? type (rnd-type rnd)))
	(let ((val (rnd-val rnd)))
	  (set-rnd-mark! rnd (get-current-trace)) ; mark choice as active
	  (if (equal? params (rnd-params rnd))
	      (begin (set! likelihood (+ likelihood (rnd-ll rnd)))
		     val)
	      ; else parameters do not match, rescore erp with new parameters
	      (let* ((l (get-ll type val params))
		    (new-rnd (make-rnd type val params l (get-current-trace))))
            (hash-table/put! rnd-table addr new-rnd) 
            (set! likelihood (+ likelihood l))
            val)));
	; not found in database, need to sample
	(let* ((val (get-sample type params))
	       (l (get-ll type val params))
	       (new-rnd (make-rnd type val params l (get-current-trace))))
	  (hash-table/put! rnd-table addr new-rnd) 
	  (set! likelihood (+ likelihood l))
	  (set! likelihood-fresh (+ likelihood-fresh l))
	  val)))) 
  

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

(define (apply-interp procedure arguments)
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
(define (lambda-body exp) (cddr exp))

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

;; modify (most) primitives to take and ignore an address variable
(define primitive-procedures
    (list (list 'flip flip)
	  (list 'bernoulli bernoulli)
	  (list 'geometric geometric)
	  (list 'cons cons)
	  (list 'car car)
	  (list 'cdr cdr)
	  (list '+ +)
	  (list '- -)
	  (list '* *)
	  (list '/ /)
	  (list '= =)
	  (list 'list list)
	  (list 'eq? eq?)
	  (list 'null? null?)))

(define (ignore-addr prim)
  (lambda (addr . args) (apply-in-underlying-scheme prim args)))

(define church-procedures
  (list (list 'church-flip church-flip)
	(list 'church-bernoulli church-bernoulli)
	(list 'church-geometric church-geometric)
	(list 'church-cons (ignore-addr cons))
	(list 'church-car (ignore-addr car))
	(list 'church-cdr (ignore-addr cdr))
	(list 'church-+ (ignore-addr +))
	(list 'church-- (ignore-addr -))
	(list 'church-* (ignore-addr *))
	(list 'church-/ (ignore-addr /))
	(list 'church-= (ignore-addr =))
	(list 'church-list? (ignore-addr list?))
	(list 'church-eq? (ignore-addr eq?))
	(list 'church-null? (ignore-addr null?))))



;(define church-procedures
; (map (lambda (x) 
; (list (symbol-append 'church- (car x)) (cadr x)))
;      primitive-procedures))

(set! primitive-procedures 
      (append primitive-procedures church-procedures))



(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
 (map (lambda (proc) (list 'primitive (cadr proc)))
      primitive-procedures))


(define (apply-primitive-procedure proc args)
      (apply-in-underlying-scheme (primitive-implementation proc) args))

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



;; generates a unique symbol (convert to and from string to simplify)
(define (gen-addr) 
  (string->symbol (symbol->string (generate-uninterned-symbol "a"))))

(define (transform-top exp) 
  (generate-uninterned-symbol 0)
  (list (append
	   '(lambda (addr))
	   (list (transform exp)))
	''(top)))
	
		
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
	((variable? exp) (symbol-append 'church- exp))
	(else exp)))


(define (transform-lambda exp)
  (let ((args (lambda-parameters exp))
	(body (lambda-body exp)))
    `(lambda ,(cons 'addr (map transform args)) ,@(map transform body))))

(define (transform-begin exp)
 (cons 'begin (map transform (begin-actions exp))))

(define (transform-let exp) 
  (let* ((bindings (let-bindings exp))
	(body (let-body exp))
	(new-bindings (map
		       (lambda (x) (list (transform (car x)) (transform (cadr x))))
		       bindings))
	(new-body (transform body)))
    (list 'let new-bindings new-body)))



(define (transform-if exp)
  (cons 'if (map transform (cdr exp))))

(define (transform-and exp)
  (cons 'and (map transform (cdr exp))))

(define (transform-or exp) 
  (cons 'or (map transform (cdr exp))))

(define (transform-definition exp) 
  (let ((texp (transform (definition-value exp)))
	(var (transform (definition-variable exp))))
    (list 'define var texp)))

(define (transform-application exp) 
  (let* ((S (gen-addr))
	 (op (operator exp))
	 (args (operands exp))
	 (top (transform op))
	 (targs (map transform args)))
    (append (list top (list 'cons `',S 'addr)) targs)))

(define (transform-primitive proc)
  (let ((impl (primitive-implementation proc))
        (new-impl (lambda (x) (apply-interp impl (cdr x))))) 
    (list 'primitive new-impl)))
  
(define (drivert-loop)
  (prompt-for-input "T>>>")
  (let ((input (read)))
    (let ((output (eval (transform-top input) the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (drivert-loop))
	
  

(define (transform-loop)
 (prompt-for-input input-prompt)
 (let ((input (read)))
   (let ((output (transform-top input)))
     (announce-output output-prompt)
     (user-print output)))
 (transform-loop))

