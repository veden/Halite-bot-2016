(module Trainer racket
  (provide (all-defined-out))

  (require threading)
  (require racket/list)
  (require "build.rkt")
  (require "cuckoo.rkt")
  
  (current-directory "/home/veden/haliteFiles/")

  (struct Outcome
    (trial
     position))
  
  (struct Trial
    (size
     bots
     seed
     solution))

  (define currentBot "cd /data/factory/repo/wkJava/halite/src; java -Xmx250m MyBot") 

  (define botPool '("cd /data/factory/repo/wkJava/halite/src/release/v2/; java MyBot"
                    "cd /data/factory/repo/wkJava/halite/src/release/v3/; java MyBot"
                    ;; "cd /data/factory/repo/wkJava/halite/src/release/v5/; java MyBot"
                    ;; "cd /data/factory/repo/wkJava/halite/src/release/v6/; java MyBot"
                    ;; "cd /data/factory/repo/wkJava/halite/src/release/v4/; java MyBot"
                    ))

  (define sizePool '(20
                     25
                     30
                     35
                     40
                     45
                     50))

  (define (pickSize)
    (list-ref sizePool (random (length sizePool))))

  (define (pickBots solution)
    (define augmentedCurrentBot (if (null? solution) currentBot
                                    (string-append currentBot (string-join (map ~v solution)))))
    (define (h c botPos acc)
      (cond ((eq? c 0) acc)
            ((eq? c botPos) (h (- c 1) botPos (cons augmentedCurrentBot acc)))
            (#t (h (- c 1) botPos (cons (list-ref botPool (random (length botPool))) acc)))))
    (let ((bots (random 2 7)))
      (h bots (random 1 (+ 1 bots)) null)))

  (define (prepTrial solution)
    (Trial (pickSize)
           (pickBots solution)
           (random 4294967087)
           solution))

  (define (cloneTrial solution newSolution)
    (Trial (Trial-size solution)
           (Trial-bots solution)
           (Trial-seed solution)
           newSolution))
  
  (define (scoreResult report)
    (define chunks (string-split report "\n"))
    
    (define (findBotIndex p c)
      (if (string=? (car p) currentBot) c
          (findBotIndex (cdr p) (+ 1 c))))
    
    (define botStanding (string-append (~v (findBotIndex chunks 1)) " "))

    (define (getPosition p)
      (string->number (second (string-split (car p) " "))))

    (if (not (eq? (string-length (string-trim (last chunks))) 0)) 0
        (~>> chunks
             (filter (lambda (x)
                       (string-prefix? x botStanding)))
             getPosition)))

  (define (showResults results)
    (string-append "Winning Score - "
                   (~v (~>> results
                            (foldl (lambda (o acc)
                                     (if (= (Outcome-position o) 0) acc
                                         (+ acc (/ (length (Trial-bots (Outcome-trial o)))
                                                   (Outcome-position o)))))
                                   0)
                            exact->inexact))
                   "\n"
                   "Won games - "
                   (~v (~>> results
                            (foldl (lambda (o acc)
                                     (if (= (Outcome-position o) 1) (+ 1 acc)
                                         acc))
                                   0)))
                   "\n"
                   "tossed games - "
                   (~v (~>> results
                            (foldl (lambda (o acc)
                                     (if (= (Outcome-position o) 0) (+ 1 acc)
                                         acc))
                                   0)))
                   "\n"
                   "total games - "
                   (~v (length results))
                   "\n"))

  (define (runTrial trial)
    (let ((args (append (list #f
                              #f
                              #f
                              "/home/veden/haliteFiles/halite"
                              (string-append "-d " (~v (Trial-size trial)) " " (~v (Trial-size trial)))
                              (string-append "-s " (~v (Trial-seed trial)))
                              "-q"
                              "-r")
                        (Trial-bots trial))))
      (let-values ([(sp i o e) (apply subprocess args)])
        (let ((report (port->string i)))
          (unless (eq? i #f)
            (close-input-port i))
          (unless (eq? o #f)
            (close-output-port o))
          (unless (eq? e #f)
            (close-input-port e))
          (Outcome trial (scoreResult report))))))
  
  (define (playGames cnt)
    (~>> (stream-map (lambda (gameNumber)
                       (runTrial (prepTrial null)))
                     (in-range cnt))
         stream->list))

  (define testSeedRounds '((2010 5)
                           (2000 10)
                           (1007 15)
                           (1010 30)
                           (1050 50)))


  (define currentBest '(0.35 0.6 0.2 0.25 0.25))
  (define currentBestScore 0.8)
  
  (define (tuneParameters testSeed)
    (let* ((gameCount (second testSeed))
           (objFunc (lambda (solution)
                      (define preppedTrials (make-list gameCount
                                                       (lambda (x)
                                                         (prepTrial solution))))
                      (define totalScore (~>> preppedTrials
                                              (map (compose length Trial-bots))
                                              (apply +)))
                      (/ (~>> preppedTrials
                              (map (lambda (trail)
                                     (let ((position (Outcome-position (runTrial trail))))
                                       (if (= position 0) 0
                                           )
                                       ))
                                   (apply ))
                              totalScore)))
           (alpha 1.5)
           (maxEpoch 1)
           (succFunc (lambda (fitness)
                       (= fitness 1.0)))
           (dropRate 0.3)
           (rangePairs (list (RangePair 0.0 1.0)
                             (RangePair 0.0 1.0)
                             (RangePair 0.0 1.0)
                             (RangePair 0.0 1.0)
                             (RangePair 0.0 1.0))))
      (cuckooSearch objFunc
                    maxEpoch
                    succFunc
                    dropRate
                    alpha
                    (vector-append #((CuckooEgg currentBest currentBestScore))
                                   (cuckooInitialize 9
                                                     rangePairs
                                                     objFunc)))))
  
  (define (playTests seedRound [solution null])
    (random-seed (car seedRound))
    (playGames (cadr seedRound)
               solution))
  
  (define (playTestSuite cnt [drp 0])
    (~>> (take (drop testSeedRounds drp) cnt)
         (map (lambda (x)
                (pretty-display x)
                (time (playTests x))))
         flatten
         showResults))

  (define (badCommand)
    (error "need commandline argument:\nsingle <int games> <int seed (-1 is random))>\ntest <int number of rounds> <?int drop first k suites>\n"))
  
  (if (<= (vector-length (current-command-line-arguments)) 1) (badCommand)
      (if (not (eq? 0 (compileCurrent))) "bad compile"
          (match (current-command-line-arguments)
            ((vector "test" r) (display (playTestSuite (string->number r))))
            ((vector "test" r d) (display (playTestSuite (string->number r)
                                                         (string->number d))))

            ((vector "single" c s) (begin (random-seed (string->number s))
                                          (display (showResults (playGames (string->number c))))))
            (_ (badCommand))))))

