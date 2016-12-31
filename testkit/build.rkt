(module Build racket
  (provide (all-defined-out))
 
  (define (cleanClasses folder recurse)
    (map (lambda (f)
           (when (path-has-extension? f "class")
             (delete-file f))
           (when (and recurse (directory-exists? f))
             (cleanClasses f recurse)))
         (directory-list folder #:build? #t)))
  
  (define (compileCurrent)
    (cleanClasses (string->path "/data/factory/repo/wkJava/halite/src/") #f)
    (cleanClasses (string->path "/data/factory/repo/wkJava/halite/src/game") #t)
    (cleanClasses (string->path "/data/factory/repo/wkJava/halite/src/logic") #t)
    (system "cd ~/haliteFiles/; rm *.log")
    (system "cd ~/haliteFiles/; rm *.hlt")
    (system/exit-code "cd /data/factory/repo/wkJava/halite/src/; javac MyBot.java")))
