(module Visualizer racket

  (require racket/gui/base)

  (struct Replay ([currentFrame #:mutable]
                  width
                  height
                  frames
                  lastFrame))

  (struct Frame (id
                 sites
                 highLows))
  
  (struct Site (x
                y 
                units
                generator
                owner
                production
                strength
                defense
                damage
                objective))

  (define activeHighlight null)

  (define (drawFrame replay layer)
    (lambda (canvas dc)
      (define frame (list-ref (Replay-frames replay) (Replay-currentFrame replay)))
      (define sites (Frame-sites frame))

      (define (scaleColor value lowHigh f)
        (define scaling (if (= 0 (second lowHigh)) 1
                            (/ 200.0 (second lowHigh))))
        (define r (if (= value 0) 0
                      (if (> (* 0.75 (- (second lowHigh) (first lowHigh))) value)
                          150
                          (if (> (* 0.50 (- (second lowHigh) (first lowHigh))) value)
                              100
                              50))))
        (define g (inexact->exact (round (* value scaling))))
        (send dc set-brush (make-object color% r g (if f (if (eq? value 0) 0 170) 0)) 'solid))
      
      (define (drawSites siteList)
        (cond ((null? siteList) null)
              (#t (begin
                    (let ((site (car siteList))
                          (scaleX (/ 500.0 (Replay-width replay)))
                          (scaleY (/ 500.0 (Replay-height replay))))
                      (cond ((eq? layer 'units) (scaleColor (Site-units site) '(0 255) #f))
                            ((eq? layer 'generator) (scaleColor (Site-generator site) (first (Frame-highLows frame)) #f))
                            ((eq? layer 'owner) (scaleColor (Site-owner site) '(0 6) #t))
                            ((eq? layer 'production) (scaleColor (Site-production site) (second (Frame-highLows frame)) #f))
                            ((eq? layer 'strength) (scaleColor (Site-strength site) (third (Frame-highLows frame)) #f))
                            ((eq? layer 'defense) (scaleColor (Site-defense site) (fourth (Frame-highLows frame)) #f))
                            ((eq? layer 'damage) (scaleColor (Site-damage site) (fifth (Frame-highLows frame)) #f)))
                      (if (and (not (null? activeHighlight))
                               (eq? (Site-x site)
                                    (Site-x activeHighlight))
                               (eq? (Site-y site)
                                    (Site-y activeHighlight)))
                          (send dc set-pen (make-object color% 255 255 255) 1 'solid)
                          (if (Site-objective site) (send dc set-pen (make-object color% 0 175 255) 1 'solid)
                              (send dc set-pen (make-object color% 0 0 0) 1 'solid)))
                      (send dc
                            draw-rectangle
                            (* (Site-x site) scaleX)
                            (* (Site-y site) scaleY)
                            scaleX
                            scaleY))
                    (drawSites (cdr siteList))))))
      (drawSites sites)))
  
  (define (buildGameBoard replay)

    (define (refresh)
      (send statusBox set-label (~v (Replay-currentFrame replay)))
      (when (not (null? activeHighlight))
        (displaySite 0 0))
      (send canvas1 refresh)
      (send canvas2 refresh)
      (send canvas3 refresh)
      (send canvas4 refresh)
      (send canvas5 refresh)
      (send canvas6 refresh)
      (send canvas7 refresh))
    
    (define frameWithEvents% (class frame%
                               (define/override (on-subwindow-char r event)
                                 (when (eq? (send event get-key-code) #\c)
                                   (exit))
                                 (when (eq? (send event get-key-code) 'right)
                                   (when (< (Replay-currentFrame replay) (Replay-lastFrame replay))
                                     (set-Replay-currentFrame! replay
                                                               (+ (Replay-currentFrame replay) 1))
                                     (refresh)))
                                 (when (eq? (send event get-key-code) 'left)
                                   (when (> (Replay-currentFrame replay) 0)
                                     (set-Replay-currentFrame! replay
                                                               (- (Replay-currentFrame replay) 1))
                                     (refresh)))
                                 (when (eq? (send event get-key-code) 'down)
                                   (if (> (Replay-currentFrame replay) 10)
                                       (set-Replay-currentFrame! replay
                                                                 (- (Replay-currentFrame replay) 10))
                                       (set-Replay-currentFrame! replay
                                                                 0))
                                   (refresh))
                                 (when (eq? (send event get-key-code) 'up)
                                   (if (< (Replay-currentFrame replay) (- (Replay-lastFrame replay) 10))
                                       (set-Replay-currentFrame! replay
                                                                 (+ (Replay-currentFrame replay) 10))
                                       (set-Replay-currentFrame! replay
                                                                 (- (Replay-lastFrame replay) 1)))
                                   (refresh))
                                 (super on-subwindow-char r event))
                               (super-new)))
    
    (define frame (new frameWithEvents%
                       [label (string-append (~v (Replay-width replay)) "x" (~v (Replay-height replay)))]
                       [width 275]
                       [height 500]
                       [x 50]
                       [y 30]))

    (define frame1 (new frameWithEvents%
                        [label "units"]
                        [width 500]
                        [height 500]
                        [x 490]
                        [y 30]))

    (define frame2 (new frameWithEvents%
                        [label "generator"]
                        [width 500]
                        [height 500]
                        [x 1000]
                        [y 30]))

    (define frame3 (new frameWithEvents%
                        [label "owner"]
                        [width 500]
                        [height 500]
                        [x 1510]
                        [y 30]))

    (define frame4 (new frameWithEvents%
                        [label "production"]
                        [width 500]
                        [height 500]
                        [x 50]
                        [y 570]))

    (define frame5 (new frameWithEvents%
                        [label "strength"]
                        [width 500]
                        [height 500]
                        [x 770]
                        [y 570]))

    (define frame6 (new frameWithEvents%
                        [label "defense"]
                        [width 500]
                        [height 500]
                        [x 1280]
                        [y 570]))

    (define frame7 (new frameWithEvents%
                        [label "damage"]
                        [width 500]
                        [height 500]
                        [x 1800]
                        [y 570]))


    (define panel (new panel%
                       [parent frame]
                       (alignment '(left top))))
    
    (new message%
         [parent panel]
         [label "Frame:"])
    
    (define statusBox (new message%
                           [parent panel]
                           [label (~v (Replay-currentFrame replay))]
                           [vert-margin 16]))
    (define siteBox (new message%
                         [parent panel]
                         [label ""]
                         [vert-margin 30]))

    (define scaleX (/ 500.0 (Replay-width replay)))
    (define scaleY (/ 500.0 (Replay-height replay)))

    (define (findSite lst xa ya f)
      (define x (if (and f (not (null? activeHighlight))) (Site-x activeHighlight)
                    (inexact->exact (floor (/ xa scaleX)))))
      (define y (if (and f (not (null? activeHighlight))) (Site-y activeHighlight)
                    (inexact->exact (floor (/ ya scaleY)))))
      (if (and (eq? x (Site-x (car lst)))
               (eq? y (Site-y (car lst))))
          (car lst)
          (findSite (cdr lst) xa ya f)))
    
    (define (displaySite x y)
      (define site (findSite (Frame-sites (list-ref (Replay-frames replay)
                                                    (Replay-currentFrame replay)))
                             x
                             y
                             #t))
      (send siteBox set-label (string-append "x-" (~v (Site-x site)) "\n"
                                             "y-" (~v (Site-y site)) "\n"
                                             "units-" (~v (Site-units site)) "\n"
                                             "generator-" (~v (Site-generator site)) "\n"
                                             "owner-" (~v (Site-owner site)) "\n"
                                             "production-" (~v (Site-production site)) "\n"
                                             "strength-" (~v (Site-strength site)) "\n"
                                             "defense-" (~v (Site-defense site)) "\n"
                                             "damage-" (~v (Site-damage site)) "\n"
                                             "objective-" (~v (Site-objective site)) "\n")))

    (define (displayHighlight x y)
      (define site (findSite (Frame-sites (list-ref (Replay-frames replay)
                                                    (Replay-currentFrame replay)))
                             x
                             y
                             #f))
      (set! activeHighlight site)
      (send statusBox set-label (~v (Replay-currentFrame replay)))
      (send canvas1 refresh)
      (send canvas2 refresh)
      (send canvas3 refresh)
      (send canvas4 refresh)
      (send canvas5 refresh)
      (send canvas6 refresh)
      (send canvas7 refresh))
    
    (define canvasWithEvents% (class canvas%
                                (define/override (on-event event)
                                  (when (eq? (send event get-event-type) 'motion) (displaySite (send event get-x)
                                                                                               (send event get-y)))
                                  (when (eq? (send event get-event-type) 'left-down) (displayHighlight (send event get-x)
                                                                                                       (send event get-y)))
                                  (when (eq? (send event get-event-type) 'right-down) (begin (set! activeHighlight null)
                                                                                             (refresh))))
                                (super-new)))

    (define canvas1 (new canvasWithEvents%
                         [parent frame1]
                         [paint-callback (drawFrame replay 'units)]))
    (define canvas2 (new canvasWithEvents%
                         [parent frame2]
                         [paint-callback (drawFrame replay 'generator)]))
    (define canvas3 (new canvasWithEvents%
                         [parent frame3]
                         [paint-callback (drawFrame replay 'owner)]))
    (define canvas4 (new canvasWithEvents%
                         [parent frame4]
                         [paint-callback (drawFrame replay 'production)]))
    (define canvas5 (new canvasWithEvents%
                         [parent frame5]
                         [paint-callback (drawFrame replay 'strength)]))
    (define canvas6 (new canvasWithEvents%
                         [parent frame6]
                         [paint-callback (drawFrame replay 'defense)]))
    (define canvas7 (new canvasWithEvents%
                         [parent frame7]
                         [paint-callback (drawFrame replay 'damage)]))

    
    (new button%
         [parent frame]
         [label "Prev Frame"]
         (callback (lambda (button event)
                     (when (> (Replay-currentFrame replay) 0)
                       (set-Replay-currentFrame! replay
                                                 (- (Replay-currentFrame replay) 1))
                       (refresh)))))
    
    
    (new button%
         [parent frame]
         [label "Next Frame"]
         (callback (lambda (button event)
                     (when (< (Replay-currentFrame replay) (Replay-lastFrame replay))
                       (set-Replay-currentFrame! replay
                                                 (+ (Replay-currentFrame replay) 1))
                       (refresh)))))

    (define jumpBox (new text-field%
                         [parent frame]
                         [label ""]))
    
    (new button%
         [parent frame]
         [label "Jump Frame"]
         (callback (lambda (button event)
                     (when (< (Replay-currentFrame replay) (Replay-lastFrame replay))
                       (set-Replay-currentFrame! replay (string->number (send jumpBox get-value)))
                       (refresh)))))
    

    (new button%
         [parent frame]
         [label "Quit"]
         (callback (lambda (button event)
                     (exit))))

    (send frame show #t)
    (send frame1 show #t)
    (send frame2 show #t)
    (send frame3 show #t)
    (send frame4 show #t)
    (send frame5 show #t)
    (send frame6 show #t)
    (send frame7 show #t))

  (define (getListIndexAsNumber lst n)
    (define a (string->number (list-ref lst n)))
    (if (eq? a #f) (begin
                     (print (list 'fuck 'nans lst n))
                     0.0)
        a))

  (define (buildSites sitesText)
    (map (lambda (siteText)
           (define chunks (string-split siteText " "))
           (Site (getListIndexAsNumber chunks 0)
                 (getListIndexAsNumber chunks 1)
                 (getListIndexAsNumber chunks 2)
                 (getListIndexAsNumber chunks 3)
                 (getListIndexAsNumber chunks 4)
                 (getListIndexAsNumber chunks 5)
                 (getListIndexAsNumber chunks 6)
                 (getListIndexAsNumber chunks 7)
                 (getListIndexAsNumber chunks 8)
                 (string=? "true" (list-ref chunks 9))))
         sitesText))

  (define (findLowHighs sites)
    (define (h s lGenerator hGenerator lProduction hProduction lStrength hStrength lDefense hDefense lDamage hDamage)
      (if (null? s) (list (list lGenerator hGenerator)
                          (list lProduction hProduction)
                          (list lStrength hStrength)
                          (list lDefense hDefense)
                          (list lDamage hDamage))
          (h (cdr s)
             (if (> lGenerator (Site-generator (car s))) (Site-generator (car s))
                 lGenerator)
             (if (< hGenerator (Site-generator (car s))) (Site-generator (car s))
                 hGenerator)
             (if (> lProduction (Site-production (car s))) (Site-production (car s))
                 lProduction)
             (if (< hProduction (Site-production (car s))) (Site-production (car s))
                 hProduction)
             (if (> lStrength (Site-strength (car s))) (Site-strength (car s))
                 lStrength)
             (if (< hStrength (Site-strength (car s))) (Site-strength (car s))
                 hStrength)
             (if (> lDefense (Site-defense (car s))) (Site-defense (car s))
                 lDefense)
             (if (< hDefense (Site-defense (car s))) (Site-defense (car s))
                 hDefense)
             (if (> lDamage (Site-damage (car s))) (Site-damage (car s))
                 lDamage)
             (if (< hDamage (Site-damage (car s))) (Site-damage (car s))
                 hDamage))))
    (h sites 1e99 -1e99 1e99 -1e99 1e99 -1e99 1e99 -1e99 1e99 -1e99))
  
  (define (frameTextToData text)
    (define frameText (string-split text "\n"))
    (define sites (buildSites (cdr frameText)))
    (Frame (string->number (car frameText))
           sites
           (findLowHighs sites)))

  (define (readReplay filePath)
    (define textFragments (string-split (call-with-input-file filePath
                                          (lambda (port)
                                            (port->string port)))
                                        "----"))
    (define mapSettings (string-split (car textFragments)
                                      " "))

    (define frameData (map frameTextToData (cdr textFragments)))
    
    (buildGameBoard (Replay 0
                            (string->number (car mapSettings))
                            (string->number (string-trim (cadr mapSettings)))
                            frameData
                            (Frame-id (car (reverse frameData))))))

  (readReplay "/data/factory/repo/wkJava/halite/src/replay.txt")
  )
