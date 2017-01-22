(module Trainer racket
  (provide (all-defined-out))

  (require threading)
  (require racket/function)
  (require racket/list)
  (require remote-shell/ssh)
  (require racket/async-channel)
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

  (define botPool '("cd /data/factory/repo/wkJava/halite/src/release/v10/; java MyBot"
                    "cd /data/factory/repo/wkJava/halite/src/release/v11/; java MyBot"
                    "cd /data/factory/repo/wkJava/halite/src/release/v12/; java MyBot"
                    "cd /data/factory/repo/wkJava/halite/src/release/v13/; java MyBot"
                    "cd /data/factory/repo/wkJava/halite/src/release/v14/; java MyBot"
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

  (define (pickBots solution botPool)
    (define (h c botPos bots acc)
      (cond ((eq? c 0) acc)
            ((eq? c botPos) (h (- c 1) botPos bots (cons currentBot acc)))
            (#t (let* ((randomIndex (random (length bots)))
                       (bot (list-ref bots randomIndex)))
                  (h (- c 1) botPos (remove bot bots) (cons bot acc))))))
    (let ((bots (random 2 7)))
      (h bots (random 1 (+ 1 bots)) botPool null)))

  (define (updateBotSolution bots solution)
    (define augmentedCurrentBot (if (null? solution) currentBot
                                    (string-append currentBot " " (string-join (map ~v solution)))))
    (for/list ((bot bots))
      (if (string=? bot currentBot)
          augmentedCurrentBot
          bot)))
  
  (define (prepTrial solution)
    (Trial (pickSize)
           (pickBots solution botPool)
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

  (define (playThreaded trials)
    (define result-channel (make-async-channel))
    (define work-channel (make-async-channel))
    (define (trialThread)
      (thread (lambda ()
                (let loop ()
                  (define trial (async-channel-get work-channel))
                  (case trial
                    ((done) 'quitting)
                    (else (async-channel-put result-channel (runTrial trial))
                          (loop)))))))
    (define threadCount 4)
    (define threads (build-list threadCount (lambda (x)
                                              (trialThread))))
    (define work-queue (append trials (make-list threadCount 'done)))
    (for ((work work-queue))
      (async-channel-put work-channel work))
    (for/list ((trial trials))
      (async-channel-get result-channel)))
  
  (define (playGames cnt)
    (playThreaded (build-list cnt (lambda (x)
                                    (prepTrial null)))))

  (define testSeedRounds '((2010 5)
                           (2000 10)
                           (1007 15)
                           (1010 30)
                           (1050 50)
                           (2060 20)
                           (2080 50)))

  (define currentBest '(0.77 0.43 0.02 0.46 0.85
                             0.69 0.70 0.27 0.64 0.34
                             0.55 0.45 0.125 0.05 0.5
                             0.3 0.2 0.1 1.3 0.12 0.2
                             2))

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
                           playThreaded
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
           (rangePairs (append (build-list 21 (lambda (x)
                                                (RangePair 0.0 1.0)))
                               (list (RangePair 0.0 8)))))
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
  
  (define (makeSnapshot name)
    (let* ((endpoint (remote #:host "halite"
                             #:user "hman"))
           (destination (string-append "/srv/halite/halite-match-manager/bots/" name "/"))
           (FQDestination (string-append "hman@halite:" destination))
           (cmd (lambda (x folder)
                  (string-append "scp " (if folder "-r " "") "/data/factory/repo/wkJava/halite/src/" x " "
                                 FQDestination))))
      (ssh endpoint (string-append "mkdir " destination))
      (system (cmd "game" #t))
      (system (cmd "logic" #t))
      (system (cmd "MyBot.java" #f))
      (system (cmd "MyBot.class" #f))
      ))

  (define (badCommand)
    (error "need commandline argument:\nsingle <int games> <int seed (-1 is random))>\ntest <int number of rounds> <?int drop first k suites>\n"))

  (if (<= (vector-length (current-command-line-arguments)) 1) (badCommand)
      (if (not (eq? 0 (compileCurrent))) "bad compile"
          (match (current-command-line-arguments)
            ((vector "test" r) (display (playTestSuite (string->number r))))
            ((vector "test" r d) (display (playTestSuite (string->number r)
                                                         (string->number d))))
            ((vector "tune" s) (pretty-display (tuneParameters (list-ref testSeedRounds (- (string->number s) 1)))))
            ((vector "snapshot" name) (pretty-display (makeSnapshot name)))
            ((vector "single" c s) (begin (when (not (= (string->number s) -1))
                                            (random-seed (string->number s)))
                                          (display (showResults (time (playGames (string->number c)))))))
            (_ (badCommand))))))

