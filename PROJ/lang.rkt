(module lang (lib "eopl.ss" "eopl") 
  
  (provide (all-defined-out))
  
  (define w-l
      (lambda (l)
        (for-each
         (lambda (e) (display e)))
        l))
  
  (define print-terms
    (lambda (terms) 
      (when (not (null? terms)) 
        (print-term (car terms)) 
         (display ", ") 
         (print-terms (cdr terms)))))
  
  (define print-term
   (lambda (t)
     (cases term t
       (a-term (id terms)
               (w-l id " (")
               (print-terms terms)
               (display ")"))
       (a-variable (var) (write var))
       (a-unification (t1 t2) (print-term t1) (display " = ") (print-term t2))
       (a-assignment (t1 t2) (print-term t1) (display " is ") (write t2))
       (a-literal (num) (write num)))))
       
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (variable
       ("$" letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (term "?" (arbno clause ".")) a-program)
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
  )
