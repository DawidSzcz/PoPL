(module queues (lib "eopl.ss" "eopl")

  (provide (all-defined-out))
  
  ;; queues

  ;; We maintain the queue by adding to the end and dequeuing from the
  ;; front. 

  ;; exercise: enqueue is expensive, since it uses append.  Do
  ;; something better than this.

  (define empty-queue
    (lambda ()
      '()))

  (define empty? null?)
  
  (define enqueue
    (lambda (q val time)
      (append q (list (list val time)))))

  (define dequeue
    (lambda (q f)
      (f (caar q) (cadar q) (cdr q))))

  )
