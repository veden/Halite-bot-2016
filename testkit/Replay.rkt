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
                 (string=? "true" (list-ref chunks 8))
                 (string=? "true" (list-ref chunks 9))
                 (string=? "true" (list-ref chunks 10))
                 (string=? "true" (list-ref chunks 11))
                 (string=? "true" (list-ref chunks 12))
                 (string=? "true" (list-ref chunks 13))
                 (string=? "true" (list-ref chunks 14))
                 (string=? "true" (list-ref chunks 15))
                 (string=? "true" (list-ref chunks 16))
                 (string=? "true" (list-ref chunks 17))
                 (string=? "true" (list-ref chunks 18))
                 (string=? "true" (list-ref chunks 19))
                 (string=? "true" (list-ref chunks 20))))
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
    (map (lambda (x)
           (match-let (((list id totalGenerator totalUnits totalPotential totalGenerated totalOverkill
                              totalDamage totalCappedLoss totalCaptured totalSites) (map string->number (string-split (string-trim x) " "))))
             (Player id totalGenerator totalUnits totalOverkill totalDamage totalPotential totalGenerated
                     totalCappedLoss totalCaptured totalSites)))
         (string-split (string-trim players) "\n")))
    
    (define (createFrame text)
      (match-let (((list stats players sites) (string-split (string-trim text) "===")))
        (match-let (((list id minExplore maxExplore minDamage maxDamage minReinforce maxReinforce
                           totalFreeGenerator) (map string->number (string-split (string-trim stats) " "))))
          (Frame id
                 (buildSites (string-split (string-trim sites) "\n"))
                 (buildPlayers players)
                 (LayerRange minExplore maxExplore)
                 (LayerRange minReinforce maxReinforce)
                 (LayerRange minDamage maxDamage)
                 totalFreeGenerator))))

    (struct Replay ([currentFrame #:mutable]
                    width
                    height
                    frames
                    lastFrame
                    generator
                    siteCounts))

    (define (replay->string replay)
      (string-join (map (lambda (x)
                          (string-append (~v (SiteCount-value x))
                                         "-"
                                         (~v (SiteCount-size x))
                                         " / "
                                         (~v (* (SiteCount-value x)
                                                (SiteCount-size x)))))
                        (Replay-siteCounts replay))
                   "\n"))

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
      (buildList (map string->number (string-split (string-trim text) " ")) null))
    
    (define (readReplay filePath)
    (define replayChunks (string-split (call-with-input-file filePath
                                         (lambda (port)
                                           (port->string port)))
                                       "----"))
    (match-let (((list settings siteCounts) (string-split (string-trim (first replayChunks)) "--")))
      (match-let (((list width height minGenerator maxGenerator) (map string->number (string-split (string-trim settings) " "))))
        (let ((frameData (map createFrame (cdr replayChunks))))
          (Replay 0
                  width
                  height
                  frameData
                  (Frame-id (car (reverse frameData)))
                  (LayerRange 0 maxGenerator)
                  (buildSiteCounts siteCounts)))))))


;; width height minProduction maxProduction--production0 count0 production1 count1...
;; ----
;; frameNumber minExplore maxExplore minDamage maxDamage minReinforce maxReinforce totalUnxploredGenerator
;; ===
;; my-id totalGenerator totalUnits totalPotential totalGenerated totalOverkill totalDamage totalCappedLoss totalCaptured totalSites
;; enemy-id ->
;;
;; |
;; V
;; ===
;; x y unit production owner explore reinforce damage isBattle isFrontier isUnexplored isInterior isBorder isField isReady isSpear isObjective
;;
;; |
;; V
;; ----
;; frame
