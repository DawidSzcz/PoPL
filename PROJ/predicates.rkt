(module lang racket
  
  (provide add-predicate initialize-predicates)
  
  (define preds 'uninitialized)
  
  (define initialize-predicates
    (lambda ()
      (set! preds (make-hash))))
  
  (define add-predicate
    (lambda (name predicate)
      (if (hash-has-key? preds)
          (hash-set! preds name (append (hash-ref! preds name) (list predicate)))
          (hash-set! preds name (list predicate)))))
  )
