(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      "test(X, Y) :- X is Y. test(3, Z)." (hash "Z" 3)
      )
    )
  )