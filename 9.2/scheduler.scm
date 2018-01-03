(module scheduler (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "queues.scm")
  (require "data-structures.scm")       ; for continuation?
  (require "lang.scm")                  ; for expval?
  
  (provide
    initialize-scheduler!
    set-final-answer! 
    
    time-expired?
    decrement-timer!

    place-on-ready-queue!
    place-on-ready-queue-yield!
    run-next-thread
    the-time-remaining
    place-on-ready-queue-mutex!

    )
  
  ;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;
  
  ;; components of the scheduler state:
  
  (define the-ready-queue   'uninitialized)         
  (define the-final-answer  'uninitialized)
  
  (define the-max-time-slice    'uninitialized)
  (define the-time-remaining    'uninitialized)

  ;; initialize-scheduler! : Int -> Unspecified
  (define initialize-scheduler!
    (lambda (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer 'uninitialized)
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice) 
      ))
  
  ;;;;;;;;;;;;;;;; the final answer ;;;;;;;;;;;;;;;;

  ;; place-on-ready-queue! : Thread -> Unspecified
  ;; Page: 184  
  (define place-on-ready-queue!
    (lambda (th)
      (set! the-ready-queue
        (enqueue the-ready-queue th the-max-time-slice))))
  
  (define place-on-ready-queue-yield!
    (lambda (th)
      (set! the-ready-queue
            (enqueue the-ready-queue th the-time-remaining))))
  
    (define place-on-ready-queue-mutex!
    (lambda (th time)
      (set! the-ready-queue
            (enqueue the-ready-queue th time))))

  ;; run-next-thread : () -> FinalAnswer
  ;; Page: 184    
  (define run-next-thread
    (lambda ()
      (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
          (lambda (first-ready-thread time-remaining other-ready-threads)
            (set! the-ready-queue other-ready-threads)            
            (set! the-time-remaining time-remaining) 
            (first-ready-thread)
            )))))

  ;; set-final-answer! : ExpVal -> Unspecified
  ;; Page: 184    
  (define set-final-answer!
    (lambda (val)
      (set! the-final-answer val)))

  ;; time-expired? : () -> Bool
  ;; Page: 184    
  (define time-expired?
    (lambda ()
      (zero? the-time-remaining)))

  ;; decrement-timer! : () -> Unspecified
  ;; Page: 184    
  (define decrement-timer!
    (lambda ()
      (set! the-time-remaining (- the-time-remaining 1))))

  )
