(module lang (lib "eopl.ss" "eopl") 
  
  (require "predicates.rkt")
  (provide process-program scan&parse)
  
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
       (variable
       ("A-Z" (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program ((arbno clause ".")) a-program)
      (clause (term ":-" (separated-list term ";") ) a-clause)
      (term (identifier "(" (separated-list term ",")")") a-term)
      (term (variable) a-variable)
      (term ("=" "(" term "," term ")") a-unification)
      (term ("is" "(" term "," expression ")") a-assignment)
      (term (number) a-literal)
      (expression ("-" "(" expression "," expression ")") a-diff)
      (expression ("+" "(" expression "," expression ")") a-add)
      (expression ("*" "(" expression "," expression ")") a-mult)
      (expression (identifier) var-exp)
      (expression (number) const-exp)   ))
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  (define process-program
    (lambda (prog)
      (initialize-predicates)
      (cases program prog
        (a-program (clauses)
                   (process-clauses clauses '())))))
  
  (define process-clauses
    (lambda (cls)
      (if (null? cls) 
          1
          (let ((cl (car cls))
                (rest (cdr cls)))
            (cases clause cl 
              (a-clause (head terms) 
                        (cases term head
                          (a-term (id args) (add-predicate id (list args terms)))
                          (else (eopl:error 'invalid-clause-head)))))))))
  )

