(module top (lib "eopl.ss" "eopl")
  
  (require "proj.rkt")
  (require "proj-init.scm")
  (require "tests.scm")
  
    (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
    (define run
    (lambda (string)
      (process-program (scan&parse string))))
  
  
    (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans correct-ans)))
  
  )