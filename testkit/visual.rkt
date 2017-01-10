(module Visualizer racket

  (require "Replay.rkt")
  (require racket/gui/base)
  (require plot)

  (define newWindowX 175)
  (define newWindowY 60)
  (define activeHighlight null)
  (define activeReplay null)
  (define scaleX 0)
  (define scaleY 0)

  (define (showVisual)
    (define frameWithEvents% (class frame%
                               (define/override (on-subwindow-char r event)
                                 (when (eq? (send event get-key-code) #\c)
                                   (exit))
                                 (when (eq? (send event get-key-code) 'right)
                                   (jumpTo activeReplay 1)
                                   (refresh))
                                 (when (eq? (send event get-key-code) 'left)
                                   (jumpTo activeReplay -1)
                                   (refresh))
                                 (when (eq? (send event get-key-code) 'down)
                                   (jumpTo activeReplay -10)
                                   (refresh))
                                 (when (eq? (send event get-key-code) 'up)
                                   (jumpTo activeReplay 10)
                                   (refresh))
                                 (super on-subwindow-char r event))
                               (super-new)))

    (define (newFrame width height [label ""])
      (let ((windowX newWindowX)
            (windowY newWindowY))
        (if (>= windowX 1700) (begin (set! newWindowX 175)
                                     (set! newWindowY (+ newWindowY 550)))
            (set! newWindowX (+ width windowX 10)))
        (new frameWithEvents%
             [label label]
             [width width]
             [height height]
             [x windowX]
             [y windowY])))
    
    (define frame (newFrame 275 500 ""))
    (define frame1 (newFrame 500 500 "units"))
    (define frame2 (newFrame 500 500 "generator"))
    (define frame3 (newFrame 500 500 "owner"))
    (define frame4 (newFrame 500 500 "explore"))
    (define frame5 (newFrame 500 500 "reinforce"))
    (define frame6 (newFrame 500 500 "damage"))
    (define frames (list frame frame1 frame2 frame3 frame4 frame5 frame6))

    (define frame9 (newFrame 200 275 "Total Generator"))
    (define frame10 (newFrame 200 275 "Total Units"))
    (define frame16 (newFrame 200 275 "Total Sites"))
    (define extendFrames (append (list frame9 frame10 frame16) frames))

    (define frame17 (newFrame 600 275 "Game metadata"))

    (define canvasWithEvents% (class canvas%
                                (define/override (on-event event)
                                  (when (eq? (send event get-event-type) 'motion) (displaySite (send event get-x)
                                                                                               (send event get-y)))
                                  (when (eq? (send event get-event-type) 'left-down) (displayHighlight (send event get-x)
                                                                                                       (send event get-y)))
                                  (when (eq? (send event get-event-type) 'right-down) (begin (set! activeHighlight null)
                                                                                             (refresh))))
                                (super-new)))

    (define (drawFrame)
      (define frame (list-ref (Replay-frames activeReplay) (Replay-currentFrame activeReplay)))
      (map (lambda (c)
             (send c suspend-flush))
           canvass)
      (map (lambda (x)
             (drawOnFrames x frame))
           (Frame-sites frame))
      (map (lambda (c)
             (send c resume-flush))
           canvass))

    (define (refreshPlots)
      (define players (Frame-players (list-ref (Replay-frames activeReplay)
                                               (Replay-currentFrame activeReplay))))
      (define (createGraphPair i p fn acc)
        (if (null? p) (reverse acc)
            (createGraphPair (+ i 1) (cdr p) fn (cons (discrete-histogram (list (fn (car p)))
                                                                          #:skip (+ (length players) 1)
                                                                          #:x-min i
                                                                          #:color (+ i 1)
                                                                          #:line-color (+ i 1))
                                                      acc))))
      (define (cgp fn)
        (createGraphPair 0
                         players
                         (lambda (x)
                           (vector (Player-id x) (fn x)))
                         null))
      
      (map (lambda (dc title data)
             (plot/dc data dc 0 10 190 260 #:x-label title #:y-label ""))
           dcPlots
           (list "total generator" "total units" "total sites")
           (list (cgp Player-totalGenerator)
                 (cgp Player-totalUnits)
                 (cgp Player-totalSites))))

    (define (newCanvas parent [o #f])
      (if o (new canvasWithEvents%
                 [parent parent]
                 [paint-callback (lambda (a b)
                                   (drawFrame))])
          (new canvasWithEvents%
               [parent parent])))

    (define canvas1 (newCanvas frame1))
    (define canvas2 (newCanvas frame2))
    (define canvas3 (newCanvas frame3))
    (define canvas4 (newCanvas frame4))
    (define canvas5 (newCanvas frame5))
    (define canvas6 (newCanvas frame6 #t))
    (define canvass (list canvas1 canvas2 canvas3 canvas4 canvas5 canvas6))

    (define canvas9 (new canvas% [parent frame9]))
    (define canvas10 (new canvas% [parent frame10]))
    (define canvas16 (new canvas% [parent frame16]
                          [paint-callback (lambda (a b)
                                            (refreshPlots))]))

    (define (findSite lst x y)
      (let ((i (car lst)))
        (if (and (eq? x (Site-x i))
                 (eq? y (Site-y i)))
            i
            (findSite (cdr lst) x y))))

    (define (refresh)
      (send statusBox set-label (~v (Replay-currentFrame activeReplay)))
      (when (not (null? activeHighlight))
        (displaySite 0 0))
      (drawFrame)
      (refreshPlots)
      (displayMetadata))

    (define (displayMetadata)
      (send siteCountsBox set-label (string-append (~v (Replay-width activeReplay)) "x" (~v (Replay-height activeReplay))
                                                   "\n"
                                                   (replay->string activeReplay)
                                                   "\tfg-"
                                                   (~v (Frame-totalFreeGenerator (list-ref (Replay-frames activeReplay)
                                                                                           (Replay-currentFrame activeReplay)))))))
    
    (define (displaySite x y)
      (define site (findSite (Frame-sites (list-ref (Replay-frames activeReplay)
                                                    (Replay-currentFrame activeReplay)))
                             (if (not (null? activeHighlight)) (Site-x activeHighlight)
                                 (inexact->exact (floor (/ x scaleX))))
                             (if (not (null? activeHighlight)) (Site-y activeHighlight)
                                 (inexact->exact (floor (/ y scaleY))))))
      (send siteBox set-label (site->string site)))

    (define (displayHighlight x y)
      (define site (findSite (Frame-sites (list-ref (Replay-frames activeReplay)
                                                    (Replay-currentFrame activeReplay)))
                             (inexact->exact (floor (/ x scaleX)))
                             (inexact->exact (floor (/ y scaleY)))))
      (set! activeHighlight site)
      (refresh))

    (define (scaleColor dc value low high f)
      (define scaling (if (= 0 high) 1
                          (/ 200.0 high)))
      (define vRange (- high low))
      (define v (if (> vRange 0) (/ (- value low) vRange)
                    0))
      (define r (if (= value 0) 0
                    (if (> 0.75 v)
                        150
                        (if (> 0.50 v)
                            100
                            50))))
      (define g (inexact->exact (round (* value scaling))))
      (send dc set-brush (make-object color% r g (if f (if (eq? value 0) 0 170) 0)) 'solid))

    (define dc1 (send canvas1 get-dc))
    (define dc2 (send canvas2 get-dc))
    (define dc3 (send canvas3 get-dc))
    (define dc4 (send canvas4 get-dc))
    (define dc5 (send canvas5 get-dc))
    (define dc6 (send canvas6 get-dc))
    (define dcs (list dc1 dc2 dc3 dc4 dc5 dc6))

    (define dc9 (send canvas9 get-dc))
    (define dc10 (send canvas10 get-dc))
    (define dc16 (send canvas16 get-dc))
    (define dcPlots (list dc9 dc10 dc16))
    
    (define (drawOnFrames site frame)
      (let ((x (* (Site-x site) scaleX))
            (y (* (Site-y site) scaleY)))
        
        (if (and (not (null? activeHighlight))
                 (eq? (Site-x site)
                      (Site-x activeHighlight))
                 (eq? (Site-y site)
                      (Site-y activeHighlight)))
            (map (lambda (dc)
                   (send dc set-pen (make-object color% 255 255 255) 1 'solid))
                 dcs)
            (cond ((Site-locked site) (map (lambda (dc)
                                             (send dc set-pen (make-object color% 255 36 113) 1 'solid))
                                           dcs))
                  ((Site-battle site) (map (lambda (dc)
                                             (send dc set-pen (make-object color% 178 36 232) 1 'solid))
                                           dcs))
                  ((Site-objective site) (map (lambda (dc)
                                                (send dc set-pen (make-object color% 255 0 162) 1 'solid))
                                              dcs)) 
                  ((Site-gate site) (map (lambda (dc)
                                           (send dc set-pen (make-object color% 36 113 255) 1 'solid))
                                         dcs))
                  (#t (map (lambda (dc)
                             (send dc set-pen (make-object color% 0 0 0) 1 'solid))
                           dcs))))

        (define (dcDraw dc property low high owner)
          (scaleColor dc (property site) low high owner)
          (send dc draw-rectangle x y scaleX scaleY))
        
        (dcDraw dc1 Site-units 0 255 #f)
        (dcDraw dc2 Site-generator 0 (LayerRange-high (Replay-generator activeReplay)) #f)
        (dcDraw dc3 Site-owner 0 6 #t)
        (dcDraw dc4 Site-explore (LayerRange-low (Frame-explore frame)) (LayerRange-high (Frame-explore frame)) #f)
        (dcDraw dc5 Site-reinforce (LayerRange-low (Frame-reinforce frame)) (LayerRange-high (Frame-reinforce frame)) #f)
        (dcDraw dc6 Site-damage (LayerRange-low (Frame-damage frame)) (LayerRange-high (Frame-damage frame)) #f)))

    (define panel (new panel%
                       [parent frame]
                       (alignment '(left top))))

    (define panelMetadata (new panel%
                               [parent frame17]
                               (alignment '(left top))))

    (new message%
         [parent panel]
         [label "Frame:"])

    (define statusBox (new message%
                           [parent panel]
                           [label (~v (Replay-currentFrame activeReplay))]
                           [vert-margin 16]))
    (define siteBox (new message%
                         [parent panel]
                         [label ""]
                         [vert-margin 30]))

    (define siteCountsBox (new message%
                               [parent panelMetadata]
                               [label ""]))
    
    (displayMetadata)
    
    (new button%
         [parent frame]
         [label "Prev Frame"]
         (callback (lambda (button event)
                     (jumpTo activeReplay -1)
                     (refresh))))

    (new button%
         [parent frame]
         [label "Next Frame"]
         (callback (lambda (button event)
                     (jumpTo activeReplay 1)
                     (refresh))))

    (define jumpBox (new text-field%
                         [parent frame]
                         [label ""]))

    (new button%
         [parent frame]
         [label "Jump Frame"]
         (callback (lambda (button event)
                     (jumpTo activeReplay (- (string->number (send jumpBox get-value)) (Replay-currentFrame activeReplay)))
                     (refresh))))

    (new button%
         [parent frame]
         [label "Quit"]
         (callback (lambda (button event)
                     (exit))))

    (map (lambda (f)
           (send f show #t))
         (cons frame17 extendFrames))
    'starting)

  (set! activeReplay (readReplay "/home/veden/haliteFiles/replay.txt"))
  (set! scaleX (/ 500.0 (Replay-width activeReplay)))
  (set! scaleY (/ 500.0 (Replay-height activeReplay)))
  (showVisual))

