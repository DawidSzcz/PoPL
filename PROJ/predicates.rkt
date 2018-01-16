(module lang racket
  
  (provide add-predicate initialize-predicates get-predicates print-predicates call-predicate )
  (require "lang.rkt")
  
  (define preds 'uninitialized)
  
  (define initialize-predicates
    (lambda ()
      (set! preds (make-hash))))
  
  (define add-predicate
    (lambda (name predicate)
      (if (hash-has-key? preds name)
          (hash-set! preds name (append (hash-ref preds name) (list predicate)))
          (hash-set! preds name (list predicate)))))
  
  (define get-predicates
    (lambda () preds))
  
  (define print-predicates
    (lambda () 
      (print-hash preds)))
  
  (define print-hash
    (lambda (hash)
      (hash-for-each 
       hash
       (lambda (name variants)
         (write name)
         (for-each 
          (lambda (variant) 
            (newline)
            (print-terms (car variant))
            (display " => ")
            (print-terms (cadr variant))
            1)
          variants)))))
  
  (define call-predicate
    (lambda (id)
      (hash-ref preds id)))
  
  )
