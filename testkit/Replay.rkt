(module Replay racket
  (provide jumpTo
           readReplay
           createFrame
           site->string
           replay->string
           (struct-out Frame)
           (struct-out Site)
           (struct-out Replay)
           (struct-out LayerRange)
           (struct-out Player)
           (struct-out SiteCount))

  (require 'threading)

  (struct Site (x
                y 
                units
                generator
                owner
                explore
                reinforce
                damage
                battle
                frontier
                unexplored
                internal
                border
                open
                ready
                spear
                objective
                combatReady
                gate
                exploreCandidate
                locked))

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
                  totalOverkill
                  totalDamage
                  totalPotential
                  totalGenerated
                  totalCappedLoss
                  totalCaptured
                  totalSites))

  (define (convertToSiteValue lst n)
    (let ((item (list-ref lst n)))
      (if (or (string=? "true" item)
              (string=? "false" item))
          (string=? "true" item)
          (let ((val (string->number ))
                (if (eq? a #f) (begin
                     (print (list 'fuck 'nans lst n))
                     0.0)
          a)))
  
  (define (buildSites sitesText)
    (map (lambda (siteText)
           (->> (string-split siteText " ")
                (map convertToSiteValue)
                (apply Site)))
         sitesText))

  (define (site->string site)
    (string-append "x-" (~v (Site-x site)) "\n"
                   "y-" (~v (Site-y site)) "\n"
                   "u-" (~v (Site-units site)) "\n"
                   "gen-" (~v (Site-generator site)) "\n"
                   "o-" (~v (Site-owner site)) "\n"
                   "exp-" (~v (Site-explore site)) "\n"
                   "eC-" (~v (Site-exploreCandidate site)) "\n"
                   "r-" (~v (Site-reinforce site)) "\n"
                   "dmg-" (~v (Site-damage site)) "\n"
                   "b-" (~v (Site-battle site)) "\n"
                   "f-" (~v (Site-frontier site)) "\n"
                   "ue-" (~v (Site-unexplored site)) "\n"
                   "in-" (~v (Site-internal site)) "\n"
                   "bor-" (~v (Site-border site)) "\n"
                   "open-" (~v (Site-open site)) "\n"
                   "gate-" (~v (Site-gate site)) "\n"
                   "rdy-" (~v (Site-ready site)) "\n"
                   "cRdy-" (~v (Site-combatReady site)) "\n"
                   "obj-" (~v (Site-objective site)) "\n"
                   "spear-" (~v (Site-spear site)) "\n"
                   "lk-" (~v (Site-locked site)) "\n"))

  (define (buildPlayers players)
    (~> (string-trim players)
        (string-split "\n")
        (map (lambda (x)
                (apply Player (~> (string-trim x)
                                  (string-split " ")
                                  (map string->number _))))
             _)))
  
  (define (createFrame text)
    (match-let* (((list stats players sites) (string-split (string-trim text) "==="))
                 ((list id minExplore maxExplore minDamage maxDamage minReinforce maxReinforce totalFreeGenerator) (-> (string-trim stats)
                                                                                                                       (string-split " ")
                                                                                                                       (map string->number _))))
      (Frame id
             (buildSites (string-split (string-trim sites) "\n"))
             (buildPlayers players)
             (LayerRange minExplore maxExplore)
             (LayerRange minReinforce maxReinforce)
             (LayerRange minDamage maxDamage)
             totalFreeGenerator)))

  (struct Replay ([currentFrame #:mutable]
                  width
                  height
                  frames
                  lastFrame
                  generator
                  siteCounts))

  (define (replay->string replay)
    (~>> (Replay-siteCounts replay)
         (map (lambda (x)
                (string-append (~v (SiteCount-value x))
                               "-"
                               (~v (SiteCount-size x))
                               " / "
                               (~v (* (SiteCount-value x)
                                      (SiteCount-size x))))))
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
    (match-let* (((list replayChunks) (~> (call-with-input-file filePath
                                            (lambda (port)
                                              (port->string port)))
                                          (string-split "----")))
                 ((list settings siteCounts) (~> (first replayChunks)
                                                 (string-trim)
                                                 (string-split "--")))
                 ((list width height minGenerator maxGenerator) (~> (string-trim settings)
                                                                    (string-split " ")
                                                                    (map string->number)))
                 ((list frameData) (map createFrame
                                        (cdr replayChunks))))
      (Replay 0
              width
              height
              frameData
              (Frame-id (last frameData))
              (LayerRange 0 maxGenerator)
              (buildSiteCounts siteCounts)))))

