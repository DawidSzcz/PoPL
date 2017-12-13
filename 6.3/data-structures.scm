(module data-structures (lib "eopl.ss" "eopl")
  
  (require "lang.scm")     
  (require "store.scm")      ; for expression?
  
  (provide (all-defined-out))               ; too many things to list
  
  ;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
  
  ;;; an expressed value is either a number, a boolean or a procval.
  
  (define-datatype expval expval?
    (num-val
     (value number?))
    (bool-val
     (boolean boolean?))
    (proc-val 
     (proc proc?))
    (ref-val
     (ref reference?)))
  
  ;;; extractors:
  
  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (else (expval-extractor-error 'num v)))))
  
  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))
  
  (define expval->proc
    (lambda (v)
      (cases expval v
        (proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))
  
  (define expval->ref
    (lambda (v)
      (cases expval v
        (ref-val (ref) ref)
        (else (expval-extractor-error 'reference v)))))
  
  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))
  
  ;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;
  
  ;; Page: 148
  (define identifier? symbol?)
 
  (define-datatype com-continuation com-continuation?               
    (block-com-cont
     (states (list-of statements?))
     (saved-env environment?)
     (saved-cont com-continuation?))
    (if-com-cont
     (state1 statement?)
     (state1 statement?)
     (saved-env environment?)
     (saved-cont com-continuation?))
    (set-com-cont
     (var identifier?)
     (saved-env environment?)
     (saved-cont com-continuation?))
    (while-com-cont 
     (exp expression?)
     (state1 statement?)
     (saved-env environment?)
     (saved-cont com-continuation?)))
  
  (define-datatype continuation continuation?
    (end-cont)                 
    (zero1-cont
     (saved-cont continuation?))
    (let-exp-cont
     (var identifier?)
     (body expression?)
     (saved-env environment?)
     (saved-cont continuation?))
    (if-test-cont 
     (exp2 expression?)
     (exp3 expression?)
     (saved-env environment?)
     (saved-cont continuation?))
    (diff1-cont                
     (exp2 expression?)
     (saved-env environment?)
     (saved-cont continuation?))
    (diff2-cont                
     (val1 expval?)
     (saved-cont continuation?))
    (rator-cont            
     (rand expression?)
     (saved-env environment?)
     (saved-cont continuation?))
    (rand-cont             
     (val1 expval?)
     (saved-cont continuation?))
    (block-cont
     (exps (list-of expression?))
     (env environment?)
     (cont continuation?))
    (set-cont
     (var identifier?)
     (env environment?)
     (cont continuation?)))
  
  ;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
  
  (define-datatype proc proc?
    (procedure
     (bvar symbol?)
     (body expression?)
     (env environment?)))
  
  ;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval reference?)                 ; new for implicit-refs
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))
  
  )
