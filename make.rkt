(module BuildScript racket
  (provide (all-defined-out))
  
  (require file/zip)

  (current-directory "/home/veden/haliteFiles/")
  
  (struct Proc
    (report
     size
     bots
     seed))

  (struct Results
    ([size20 #:mutable]
     [size25 #:mutable]
     [size30 #:mutable]
     [size35 #:mutable]
     [size40 #:mutable]
     [size45 #:mutable]
     [size50 #:mutable]))

  (struct ResultRecord ([wins #:mutable]
                        [loss #:mutable]
                        [tossed #:mutable]))

  (define currentBot "cd /data/factory/repo/wkJava/halite/src/; java -Xmx250m MyBot")

  (define botPool  '("cd /data/factory/repo/wkJava/halite/src/v1; java RandomBot"
                     "cd /data/factory/repo/wkJava/halite/src/v2; java MyBot"
                     "cd /data/factory/repo/wkJava/halite/src/v3; java MyBot"))

  (define sizePool '(20
                     25
                     30
                     35
                     40
                     45
                     50))

  (define (pickSize)
    (list-ref sizePool (random (length sizePool))))

  (define (pickBots)
    (define (h c botPos acc)
      (cond ((eq? c 0) acc)
            ((eq? c botPos) (h (- c 1) botPos (cons currentBot acc)))
            (#t (h (- c 1) botPos (cons (list-ref botPool (random (length botPool))) acc)))))
    (let ((bots (random 2 7)))
      (h bots (random 1 (+ 1 bots)) null)))

  (define (runInstance)
    (let* ((size (pickSize))
           (seed (random 4294967087))
           (bots (pickBots))
           (args (append (list #f
                               #f
                               #f
                               "/home/veden/haliteFiles/halite"
                               (string-append "-d " (~v size) " " (~v size))
                               (string-append "-s " (~v seed))
                               "-q"
                               "-t")
                         bots)))
      (let-values ([(sp i o e) (apply subprocess args)])
        (let ((report (port->string i)))
          (unless (eq? i #f)
            (close-input-port i))
          (unless (eq? o #f)
            (close-output-port o))
          (unless (eq? e #f)
            (close-input-port e))
          (Proc report size bots seed)))))

  (define (compileCurrent)
    (system "cd /data/factory/repo/wkJava/halite/src/; rm *.class")
    (system "cd ~/haliteFiles/; rm *.log")
    (system "cd ~/haliteFiles/; rm *.hlt")
    (system/exit-code "cd /data/factory/repo/wkJava/halite/src/; javac MyBot.java"))

  (define (scoreResult report results)
    (define chunks (string-split report "\n"))
    
    (define (findBotIndex p c)
      (if (string=? (car p) currentBot) c
          (findBotIndex (cdr p) (+ 1 c))))
    
    (define botStanding (string-append (~v (findBotIndex chunks 1)) " "))
    
    (define (didWin? p)
      (if (string-prefix? (car p) botStanding) (string-suffix? (car p) "1")
          (didWin? (cdr p))))

    (define (isTossed? p)
      (if (null? (cdr p)) (not (eq? (string-length (string-trim (car p))) 0))
          (isTossed? (cdr p))))

    (cond ((isTossed? chunks) (set-ResultRecord-tossed! results (+ 1 (ResultRecord-tossed results))))
          ((didWin? chunks) (set-ResultRecord-wins! results (+ 1 (ResultRecord-wins results))))
          (#t (set-ResultRecord-loss! results (+ 1 (ResultRecord-loss results)))))

    results)

  (define (totalRecord record)
    (+ (ResultRecord-wins record)
       (ResultRecord-loss record)
       (ResultRecord-tossed record)))

  (define (showRecord record)
    (define total (totalRecord record))
    (define winPercent (if (> total 0) (/ (ResultRecord-wins record) total)
                           0))
    
    (list (list "wins-" (ResultRecord-wins record))
          (list "loss-" (ResultRecord-loss record))
          (list "tossed-" (ResultRecord-tossed record))
          (list (exact->inexact winPercent) "%")))

  (define (showPlays results)
    (define total (+ (totalRecord (Results-size20 results))
                     (totalRecord (Results-size25 results))
                     (totalRecord (Results-size30 results))
                     (totalRecord (Results-size35 results))
                     (totalRecord (Results-size40 results))
                     (totalRecord (Results-size45 results))
                     (totalRecord (Results-size50 results))))
    (define totalWins (+ (ResultRecord-wins (Results-size20 results))
                         (ResultRecord-wins (Results-size25 results))
                         (ResultRecord-wins (Results-size30 results))
                         (ResultRecord-wins (Results-size35 results))
                         (ResultRecord-wins (Results-size40 results))
                         (ResultRecord-wins (Results-size45 results))
                         (ResultRecord-wins (Results-size50 results))))

    (define winPercent (if (> total 0) (/ totalWins total)
                           0))
    
    (list (list "size20" (showRecord (Results-size20 results)))
          (list "size25" (showRecord (Results-size25 results)))
          (list "size30" (showRecord (Results-size30 results)))
          (list "size35" (showRecord (Results-size35 results)))
          (list "size40" (showRecord (Results-size40 results)))
          (list "size45" (showRecord (Results-size45 results)))
          (list "size50" (showRecord (Results-size50 results)))
          (list (exact->inexact winPercent) "%")))

  (define (recordGame results game)      
    (cond ((eq? (Proc-size game) 20) (set-Results-size20! results (scoreResult (Proc-report game) (Results-size20 results))))
          ((eq? (Proc-size game) 25) (set-Results-size25! results (scoreResult (Proc-report game) (Results-size25 results))))
          ((eq? (Proc-size game) 30) (set-Results-size30! results (scoreResult (Proc-report game) (Results-size30 results))))
          ((eq? (Proc-size game) 35) (set-Results-size35! results (scoreResult (Proc-report game) (Results-size35 results))))
          ((eq? (Proc-size game) 40) (set-Results-size40! results (scoreResult (Proc-report game) (Results-size40 results))))
          ((eq? (Proc-size game) 45) (set-Results-size45! results (scoreResult (Proc-report game) (Results-size45 results))))
          ((eq? (Proc-size game) 50) (set-Results-size50! results (scoreResult (Proc-report game) (Results-size50 results)))))
    results)

  (define (playGames count)
    (define (h c results)
      (cond ((eq? c 0) results)
            (#t (h (- c 1) (recordGame results (runInstance))))))
    (showPlays (h count (Results (ResultRecord 0 0 0)
                                 (ResultRecord 0 0 0)
                                 (ResultRecord 0 0 0)
                                 (ResultRecord 0 0 0)
                                 (ResultRecord 0 0 0)
                                 (ResultRecord 0 0 0)
                                 (ResultRecord 0 0 0)))))

  (if (eq? (vector-length (current-command-line-arguments)) 0) "need commandline argument - series <int games> <int seed (-1 is random))>"
      (let ((arg (vector-ref (current-command-line-arguments) 0)))
        (if (not (eq? 0 (compileCurrent))) "bad compile"
            (cond  ((string=? arg "series") (begin
                                              ;;                                             (random-seed 1234)
                                              (if (> (vector-length (current-command-line-arguments)) 2) 
                                                  (let ((arg1 (vector-ref (current-command-line-arguments) 1))
                                                        (arg2 (vector-ref (current-command-line-arguments) 2)))
                                                    (unless (eq? (string->number arg2) -1)
                                                      (random-seed (string->number arg2)))
                                                    (pretty-display (playGames (string->number arg1))))
                                                  "you need a number of rounds"))))))))

