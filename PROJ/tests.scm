(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
      (list "test(3, $z) ? test($x, $y) :- is($y, $z)." (hash "Z" 3))
      (list "human($x, $y) ? human(24, 12) :-." (hash (a-variable '$x) (a-literal 24) (a-variable '$y) (a-literal 12))                                  
      )
    )
  )

(process-program(scan&parse "human($x) ? human(24) :-."  ))
(process-program(scan&parse "test(3, $z) ? test($x, $y) :- is($y, $z)."  ))