(module Trainer racket
  (provide (all-defined-out))

  (require threading)
  (require racket/function)
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

  (define botPool '(;; "cd /data/factory/repo/wkJava/halite/src/release/s1/; java MyBot"
                    ;; "cd /data/factory/repo/wkJava/halite/src/release/s2/; java MyBot"
                    "cd /data/factory/repo/wkJava/halite/src/release/v2/; java MyBot"
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
    (define (h c botPos acc)
      (cond ((eq? c 0) acc)
            ((eq? c botPos) (h (- c 1) botPos (cons currentBot acc)))
            (#t (h (- c 1) botPos (cons (list-ref botPool (random (length botPool))) acc)))))
    (let ((bots (random 2 7)))
      (h bots (random 1 (+ 1 bots)) null)))

  (define (updateBotSolution bots solution)
    (define augmentedCurrentBot (if (null? solution) currentBot
                                    (string-append currentBot " " (string-join (map ~v solution)))))
    (for/list ((bot bots))
      (if (string=? bot currentBot)
          augmentedCurrentBot
          bot)))
  
  (define (prepTrial solution)
    (Trial (pickSize)
           (pickBots solution)
           (random 4294967087)
           solution))

  (define (cloneTrial newSolution solution)
    (Trial (Trial-size solution)
           (updateBotSolution (Trial-bots solution) newSolution)
           (Trial-seed solution)
           newSolution))

    (define (scoreResult report)
      (define chunks (string-split report "\n"))
      
      (define (findBotIndex p c)
        (if (string-prefix? (car p) currentBot) c
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
                     "Max Score - "
                     (~v (~>> results
                              (map (compose length Trial-bots Outcome-trial))
                              (apply +)))
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
                                "-r"
                                )
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

  (define currentBest '(0.69 0.53 0.3 0.38 0.78))
    
  (define (tuneParameters testSeed)
    (random-seed (first testSeed))
    (let* ((gameCount (second testSeed))
           (trials (build-list gameCount
                               (lambda (x)
                                 (prepTrial null))))
           (objFunc (lambda (solution)
                      (define totalScore (~>> trials
                                              (map (compose length Trial-bots))
                                              (apply +)))
                      (~>> trials
                           (map (curry cloneTrial solution))
                           (map runTrial)
                           (foldl (lambda (o acc)
                                    (if (= (Outcome-position o) 0) acc
                                        (+ acc (/ (length (Trial-bots (Outcome-trial o)))
                                                  (Outcome-position o)))))
                                  0)
                           (/ _ totalScore))))
           (alpha 1.8)
           (maxEpoch 10)
           (succFunc (lambda (fitness)
                       (= fitness 1.0)))
           (dropRate 0.3)
           (rangePairs (list (RangePair 0.0 1.0)
                             (RangePair 0.0 1.0)
                             (RangePair 0.0 1.0)
                             (RangePair 0.0 1.0)
                             (RangePair 0.0 1.0))))
      (vector-map cuckooEgg->list (cuckooSearch objFunc
                                                maxEpoch
                                                succFunc
                                                dropRate
                                                rangePairs
                                                alpha
                                                (vector-append (vector (CuckooEgg currentBest (objFunc currentBest)))
                                                               (cuckooInitialize 9
                                                                                 rangePairs
                                                                                 objFunc))))))


  (define (playTests seedRound [solution null])
    (random-seed (car seedRound))
    (playGames (cadr seedRound)))

  (define (playTestSuite cnt [drp 0])
    (~>> (take (drop testSeedRounds drp) cnt)
         (map (lambda (x)
                (pretty-display x)
                (let ((result (time (playTests x))))
                  (pretty-display (showResults result))
                  result)))
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
            ((vector "tune" s) (pretty-display (tuneParameters (list-ref testSeedRounds (- (string->number s) 1)))))
            ((vector "single" c s) (begin (random-seed (string->number s))
                                          (display (showResults (time (playGames (string->number c)))))))
            (_ (badCommand))))))

