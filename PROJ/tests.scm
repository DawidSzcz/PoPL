(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      "test(X(), Y()) :- is(X(), 1). test(3, Z()):-." (hash "Z" 3)
      )
    )
  )