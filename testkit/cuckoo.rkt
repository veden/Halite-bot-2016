(module Cuckoo racket
  (provide cuckooSearch
           cuckooInitialize
           (struct-out RangePair))
  (random-seed 100)

  (require racket/gui/base)
  (require threading)

  (define (levyFlight xs rangePairs alpha) 
    (define scaling (expt (random)
                          (/ -1
                             alpha)))
    (map (lambda (x rp)
           (let ((v (+ x (* scaling (cos (* (random) 2 pi))))))
             (if (< (RangePair-low rp) v (RangePair-high rp)) v
                 x)))
         xs
         rangePairs))

  
  (define (randomRange low high)
    (+ low (* (random)
              (- high low))))

  
  (struct CuckooEgg (solution fitness))
  (struct RangePair (low high))

  
  (define (cuckooEgg->list c)
    (match c
      ((CuckooEgg solution fitness) (list fitness solution))))

  
  (define (pickSolution solutions)
    (~>> (random (vector-length solutions))
         (vector-ref solutions)
         CuckooEgg-solution))

  
  (define (cuckooCloneSolution solutions alpha rangePairs objFunc)
    (let ((newSolution (levyFlight (pickSolution solutions) rangePairs alpha)))
      (CuckooEgg newSolution
                 (objFunc newSolution))))

  
  (define (cuckooGenerateSolution rangePairs objFunc)
    (let ((newSolution (for/list ((rangePair rangePairs))
                         (randomRange (RangePair-low rangePair)
                                      (RangePair-high rangePair)))))
      (CuckooEgg newSolution (objFunc newSolution))))

  
  (define (cuckooBetter fitness solution)
    (match solution
      ((CuckooEgg _ targetFitness) (> fitness targetFitness))))

  
  (define (cuckooInjectSolution newSolution nests)
    (let* ((nestCount (vector-length nests))
           (targetNestIndex (random nestCount)))
      (for/vector ((i nestCount)
                   (nest nests))
        (if (and (= i targetNestIndex) (cuckooBetter (CuckooEgg-fitness newSolution) nest))
            newSolution
            nest))))

  
  (define (cuckooSortSolutions solutions)
    (vector-sort solutions > #:key CuckooEgg-fitness))

  
  (define (cuckooCullSolutions dropRate rangePairs objFunc solutions)
    (let* ((nestCount (vector-length solutions))
           (lastIndex (- nestCount (inexact->exact (floor (* nestCount dropRate)))))
           (neededSolutions (- nestCount lastIndex)))
      (vector-append (vector-take solutions lastIndex)
                     (build-vector neededSolutions (lambda (x)
                                                     (cuckooGenerateSolution rangePairs objFunc))))))

  
  (define (cuckooSearch objFunc maxEpoch succFunc dropRate rangePairs alpha solutions)
    (~>> (let cs ((epoch 0)
                  (nests solutions))
           (if (or (= maxEpoch epoch) (succFunc nests)) nests
               (cs (+ epoch 1)
                   (~>> nests
                        (cuckooInjectSolution (cuckooCloneSolution nests alpha rangePairs objFunc))
                        cuckooSortSolutions
                        (cuckooCullSolutions dropRate rangePairs objFunc)))))
         cuckooSortSolutions))

  
  (define (cuckooInitialize nests rangePairs objFunc)
    (build-vector nests (lambda (x)
                          (cuckooGenerateSolution rangePairs objFunc))))

  (define (debugLevy) 
    (define (fly)
      (define p (list '(25 25)))
      (let f ((i 0)
              (acc p))
                      (if (>= i 10000) (reverse acc)
                          (f (+ 1 i)
                             (cons (levyFlight (car acc)
                                               (list (RangePair 0.0 450.0)
                                                     (RangePair 0.0 450.0))
                                               1.7)
                                   acc)))))
    
    (define frame1 (new frame%
                        [label "plot"]
                        [width 700]
                        [height 700]
                        [x 40]
                        [y 40]))
    (define canvas1 (new canvas%
                         [parent frame1]
                         [paint-callback (lambda (a b)
                                           (send dc1 set-pen (make-object color% 0 0 0) 1 'solid)
                                           (send dc1 set-brush (make-object color% 0 0 0) 'solid)
                                           (send dc1 draw-lines (map (lambda (x)
                                                                       (match x
                                                                         ((list x y) (cons x y))))
                                                                     (fly)))
                                           )]))

    (define dc1 (send canvas1 get-dc))

    (send frame1 show #t))

  
  (define (testCuckoo)
    (let ((objFunc (lambda (solution)
                     (- 1 (foldl (lambda (x acc)
                                   (+ acc (* x x)))
                                 0
                                 solution))))
          (alpha 1.5)
          (maxEpochs 100)
          (succFunc (lambda (nests)
                      #f))
          (dropRate 0.3)
          (rangePairs (list (RangePair -5.12 5.12)
                            (RangePair -5.12 5.12)
                            (RangePair -5.12 5.12)
                            (RangePair -5.12 5.12))))
      (~>> (cuckooSearch objFunc
                         maxEpochs
                         succFunc
                         dropRate
                         rangePairs
                         alpha
                         (cuckooInitialize 10
                                           rangePairs
                                           objFunc))
           (vector-map cuckooEgg->list)))))

