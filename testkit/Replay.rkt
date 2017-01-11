(module Replay racket
  (provide jumpTo
           readReplay
           createFrame
           site->string1
           site->string2
           replay->string
           (struct-out Frame)
           (struct-out Site)
           (struct-out Replay)
           (struct-out LayerRange)
           (struct-out Player)
           (struct-out SiteCount))

  (require threading)
  (require racket/function)
  
  (struct Site (x
                y
                generator
                units
                owner
                explore
                reinforce
                damage
                locked
                age
                accumulator
                exploreValue
                battle
                frontier
                unexplored
                internal
                border
                open
                ready
                objective
                combatReady
                gate))

  (struct SiteCount (value size))
  
  (struct LayerRange (low high))
  
  (struct Frame (id
                 sites
                 players
                 explore
                 reinforce
                 damage
                 totalFreeGenerator))

  (struct Player (id
                  totalGenerator
                  totalUnits
                  totalSites))

  (struct Replay ([currentFrame #:mutable]
                  width
                  height
                  frames
                  lastFrame
                  generator
                  siteCounts))
  
  (define (convertToSiteValue item)
    (if (or (string=? "true" item)
            (string=? "false" item))
        (string=? "true" item)
        (let ((val (string->number item)))
          (if (eq? val #f) (begin
                             (error item))
              0.0)
          val)))

  (define (makeSite generator siteText)
    (match-let (((list x y generator) (~> (string-trim generator)
                                          (string-split " ")
                                          (map convertToSiteValue _))))
      (let* ((rawValues (~>> (string-trim siteText)
                             (string-split _ " ")
                             (map convertToSiteValue)))
             (flags (let procFlags ((remaining (last rawValues))
                                    (acc null))
                      (if (= remaining 1)
                          (reverse acc)
                          (procFlags (arithmetic-shift remaining -1)
                                     (cons (bitwise-bit-set? remaining 0)
                                           acc))))))
        (~>> (drop-right rawValues 1)
             (append (list x y generator) _ flags)
             (apply Site)))))
  
  (define (buildSites generators sitesText)
    (map makeSite
         generators
         sitesText))

  (define (site->string1 site)
    (string-append "x-" (~v (Site-x site)) "\n"
                   "y-" (~v (Site-y site)) "\n"
                   "ge-" (~v (Site-generator site)) "\n"
                   "u-" (~v (Site-units site)) "\n"
                   "o-" (~v (Site-owner site)) "\n"
                   "exp-" (~v (Site-explore site)) "\n"
                   "r-" (~v (Site-reinforce site)) "\n"
                   "dmg-" (~v (Site-damage site)) "\n"
                   "lk-" (~v (Site-locked site)) "\n"
                   "b-" (~v (Site-battle site)) "\n"
                   "f-" (~v (Site-frontier site)) "\n"
                   "ue-" (~v (Site-unexplored site)) "\n"
                   "in-" (~v (Site-internal site)) "\n"
                   "bor-" (~v (Site-border site)) "\n"
                   "op" (~v (Site-open site)) "\n"
                   "g-" (~v (Site-gate site)) "\n"
                   "rdy-" (~v (Site-ready site)) "\n"
                   "cRdy-" (~v (Site-combatReady site)) "\n"
                   "obj-" (~v (Site-objective site)) "\n"))

  (define (site->string2 site)
    (string-append "age-" (~v (Site-age site)) "\n"
                   "eV-" (~v (Site-exploreValue site)) "\n"
                   "acc-" (~v (Site-accumulator site)) "\n"))

  (define (buildPlayers players)
    (~> (string-trim players)
        (string-split "\n")
        (map (lambda (x)
               (apply Player (~> (string-trim x)
                                 (string-split " ")
                                 (map string->number _))))
             _)))

  (define (createFrame generators text)
    (match-let* (((list stats players sites) (string-split (string-trim text) "==="))
                 ((list id
                        minExplore maxExplore
                        minDamage maxDamage
                        minReinforce maxReinforce
                        totalFreeGenerator) (~> (string-trim stats)
                                                (string-split " ")
                                                (map string->number _))))
      (Frame id
             (buildSites generators (string-split (string-trim sites) "\n"))
             (buildPlayers players)
             (LayerRange minExplore maxExplore)
             (LayerRange minReinforce maxReinforce)
             (LayerRange minDamage maxDamage)
             totalFreeGenerator)))

  (define (replay->string replay)
    (~>> (Replay-siteCounts replay)
         (map (lambda (x)
                (string-append (~v (inexact->exact (SiteCount-value x)))
                               "-"
                               (~v (inexact->exact (SiteCount-size x)))
                               "/"
                               (~v (inexact->exact (* (SiteCount-value x)
                                                      (SiteCount-size x)))))))
         (string-join _ "\n")))

  (define (jumpTo replay offset)
    (set-Replay-currentFrame! replay
                              (let ((index (+ (Replay-currentFrame replay) offset)))
                                (if (< index 0) 0
                                    (if (>= index (Replay-lastFrame replay)) (Replay-lastFrame replay)
                                        index))))
    replay)

  (define (buildSiteCounts text)
    (define (buildList c acc)
      (if (null? c) (reverse acc)
          (buildList (cddr c) (cons (SiteCount (car c)
                                               (cadr c))
                                    acc))))
    (~> (string-trim text)
        (string-split " ")
        (map string->number _)
        (buildList null)))

  (define (readReplay filePath)
    (let* ((replayChunks (~> (call-with-input-file filePath
                               (lambda (port)
                                 (port->string port)))
                             (string-split "----")))
           (header (~> (first replayChunks)
                       (string-split "\n")))
           (siteFixedAttributes (cdr header)))
      (match-let* (((list settings siteCounts) (~> (first header)
                                                   string-trim
                                                   (string-split "--")))
                   ((list width height minGenerator maxGenerator) (~> (string-trim settings)
                                                                      (string-split " ")
                                                                      (map string->number _))))
        (let ((frameData (map (curry createFrame
                                     siteFixedAttributes)
                              (cdr replayChunks))))
          (Replay 0
                  width
                  height
                  frameData
                  (Frame-id (last frameData))
                  (LayerRange 0 maxGenerator)
                  (buildSiteCounts siteCounts)))))))

