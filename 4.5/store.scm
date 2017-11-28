(module store (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require racket/vector)
   
  (provide initialize-store! reference? newref deref setref!
    instrument-newref get-store-as-list)
  
  (define instrument-newref (make-parameter #t))
  
  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;
  
  ;;; world's dumbest model of the store:  the store is a list and a
  ;;; reference is number which denotes a position in the list.

  ;; the-store: a Scheme variable containing the current state of the
  ;; store.  Initially set to a dummy variable.
  ;;;;(define the-store 'uninitialized)

  ;; empty-store : () -> Sto
  ;; Page: 111
  (define empty-store
    (lambda () (make-vector 0)))
  
  ;; initialize-store! : () -> Sto
  ;; usage: (initialize-store!) sets the-store to the empty-store
  ;; Page 111
  (define initialize-store!
    (lambda ()
      (empty-store)))

  ;; get-store : () -> Sto
  ;; Page: 111
  ;; This is obsolete.  Replaced by get-store-as-list below
  ;;;;(define get-store
  ;;;;  (lambda () the-store))

  ;; reference? : SchemeVal -> Bool
  ;; Page: 111
  (define reference?
    (lambda (v)
      (integer? v)))

  ;; newref : ExpVal -> Ref
  ;; Page: 111
  (define newref
    (lambda (the-store val)
      (let ((next-ref (vector-length the-store)))
        (list next-ref (vector-append the-store (list->vector (list val)))))))                                         

  ;; deref : Ref -> ExpVal
  ;; Page 111
  (define deref 
    (lambda (the-store ref)
      (vector-ref the-store ref)))

  ;; setref! : Ref * ExpVal -> Unspecified
  ;; Page: 112
  (define setref!                       
    (lambda (the-store ref val)
      (vector-set! the-store ref val)
      the-store))

  ;; get-store-as-list : () -> Listof(List(Ref,Expval))
  ;; Exports the current state of the store as a scheme list.
  ;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
  ;;   where foo, bar, and baz are expvals.
  ;; If the store were represented in a different way, this would be
  ;; replaced by something cleverer.
  ;; Replaces get-store (p. 111)
   (define get-store-as-list
     (lambda (the-store)
       (letrec
         ((inner-loop
            ;; convert sto to list as if its car was location n
            (lambda (n)
              (if (eqv? n (vector-length the-store))
                '()
                (cons
                  (list n (vector-ref the-store n))
                  (inner-loop (+ n 1)))))))
         (inner-loop 0))))

  )
