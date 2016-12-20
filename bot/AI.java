package bot;

import java.util.Collections;
import java.util.Comparator;
import java.util.Random;
import java.util.function.Predicate;

import bot.model.Entity;
import bot.util.MoveUtils;
import bot.util.RingIterator;

import game.GameMap;
import game.Site;
import game.Site.Direction;
import game.Site.State;
import game.Stats;

public class AI extends Entity {

    private Predicate<Site> pMine = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return t.get(State.MINE);
	    }
	};

    private Predicate<Site> pObjective = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return t.get(State.OBJECTIVE);
	    }
	};

    private Predicate<Site> pMaxUnits = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return t.units == 255;
	    }
	};
    
    private Comparator<Site> minUnitsCompare = new Comparator<Site>() {
	    @Override
	    public int compare(Site arg0, Site arg1) {
		int v = arg0.units - arg1.units;
		if (v == 0)
		    return arg0.id - arg1.id;
		return v;
	    }
	};

    private Comparator<Site> maxExploreCompare = new Comparator<Site>() {
	    @Override
	    public int compare(Site o1, Site o2) {
		float v = o2.explore - o1.explore;
		if (v == 0)
		    return o1.id - o2.id;
		return v > 0 ? 1 : -1;
	    }
	};
    
    public AI(byte id, GameMap map) {
	super(id, map);
    }
                
    public void planTroopMovements() {
	float mapScaling = (1 - ((float)map.unexplored.size() / map.totalSites));
	float defenseRange = 1 + (map.scaler * (1 - mapScaling));
	
	for (Site s : battles) {
	    float highest = -Float.MAX_VALUE;
	    for (Site neighbor : s.neighbors.values())
		if (neighbor.get(State.NEUTRAL) && neighbor.get(State.BATTLE) && (neighbor.damage > highest) && (neighbor.damage != 0))
		    highest = neighbor.damage;
	    if (highest != Float.MAX_VALUE) {
		s.damage = 0.95f * highest;
		RingIterator ri = new RingIterator(s, pMine);
		for (int d = 0; d < defenseRange && ri.hasNext(); d++) {
		    for (Site r : ri.next()) {
			for (Site n : r.neighbors.values()) {
			    float v = 0.90f * n.damage;
			    if (v > r.damage)
				r.damage = v;
			}
		    }
		}
	    }
	}

	for (Site s : frontier) {
	    for (Site neighbor : s.neighbors.values())
		if (neighbor.get(State.MINE) && (neighbor.damage == 0)) {
		    float v = s.explore;
		    if (v > neighbor.reinforce)
			neighbor.reinforce = v;
		}
	}

	float spearDiffusion = Math.max(0.90f - mapScaling, 0.5f);
	for (Site s : spears) {
	    RingIterator ri = new RingIterator(s, pMine);
	    for (int d = 0; d < 8  && ri.hasNext(); d++) {
		for (Site r : ri.next()) {
		    for (Site n : r.neighbors.values()) {
			float v = spearDiffusion * n.reinforce;
			if (v > r.reinforce)
			    r.reinforce = v;
		    }
		}
	    }
	}
	float borderDiffusion = Math.min(0.5f + mapScaling, 0.90f);
	for (Site s : border) {
	    RingIterator ri = new RingIterator(s, pMine);
	    for (int d = 0; d < 15 && ri.hasNext(); d++) {
		RingIterator ro = new RingIterator(s, pObjective);
		float scale = borderDiffusion;
		if (ro.next().size() != 0)
		    scale = 0.99f;
		for (Site r : ri.next()) {
		    for (Site n : r.neighbors.values()) {
			float v = scale * n.reinforce;
			if (v > r.reinforce)
			    r.reinforce = v;
		    }
		}
	    }
	}

	
    }

    private boolean captureObjective(Site s) {
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (neighbor.get(State.OBJECTIVE) && MoveUtils.validExplore(s, neighbor, false) &&
		(((s.moving() && (s.target().explore < neighbor.explore))) ||
		 (!s.moving())))
		s.heading = d;
	}
	return s.moving();
    }

    private boolean reinforceDefense(Site s) {
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (MoveUtils.validMove(s, neighbor) && (s.damage < neighbor.damage))
		s.heading = d;
	}
	return s.moving();
    }

    private boolean exploreSite(Site s) {
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (s.target().explore < neighbor.explore)
		s.heading = d;
	}
	if (!MoveUtils.validExplore(s, s.target(), false))
	    s.heading = Direction.STILL;

	return s.moving();
    }

    private boolean reinforceExplorer(Site s) {
	if (s.get(State.READY) && !s.moving())
	    for (Direction d : Site.CARDINALS) {
		Site neighbor = s.neighbors.get(d);
		if (MoveUtils.validMove(s, neighbor) && (s.target().reinforce < neighbor.reinforce))
		    s.heading = d;
	    }
	return s.moving();
    }

    private boolean captureSite(Site s) {
	float highestDamage = 0;
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (neighbor.damage > highestDamage)
		highestDamage = neighbor.damage;
	    if (MoveUtils.validCapture(s, neighbor) && ((s.damage <= neighbor.damage) || 
							(s.units <= Stats.maxGenerator)))
		s.heading = d;
	}
	if ((s.units > Stats.maxGenerator) && (highestDamage != s.target().damage))
	    s.heading = Direction.STILL;
	return s.moving();
    }
    
    public void move() {
	Collections.sort(battles, minUnitsCompare);
	
	for (Site s : battles) {
	    if (!captureSite(s) && s.get(State.COMBAT_READY)) {
		float best = -Float.MAX_VALUE;
		for (Direction d : Site.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);
		    if (MoveUtils.validAttack(s, neighbor)) {
			float temp = s.units > 90 ? MoveUtils.totalGenerator(neighbor) : MoveUtils.totalDeath(neighbor);
			if ((s.damage < neighbor.damage) && (best <= temp)) {
			    best = temp;
			    s.heading = d;
			}
		    }
		}
		
		if (s.moving()) {
		    for (Site n : new RingIterator(s.target()).next())
			    n.damage = 0.0f;
		} else
		    reinforceDefense(s);
	    }
	    MoveUtils.moveSiteToSite(s, s.target());
	}

	Collections.sort(frontier, maxExploreCompare);
	for (Site s : frontier) {
	    if (MoveUtils.validJoint(s, true))
		for (Direction d : Site.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);	
		    if (neighbor.get(State.MINE) && MoveUtils.validExplore(neighbor, s, true)) {
			neighbor.heading = Site.reverse(d);
			MoveUtils.moveSiteToSite(neighbor, s);
		    }
		}
	}
	
	Collections.sort(spears, minUnitsCompare); 	
	for (Site s : spears) {
	    if (s.damage == 0) {
		if (!captureObjective(s))
		    if (!s.get(State.READY) || !reinforceExplorer(s))
			exploreSite(s);
	    } else if (s.get(State.READY))
		reinforceDefense(s);
	    
	    MoveUtils.moveSiteToSite(s, s.target());
	}
	
	Collections.sort(border, minUnitsCompare);	
	for (Site s : border) {
	    if (s.damage == 0) {
		if (!captureObjective(s))
		    if (!exploreSite(s) && s.get(State.READY))
			reinforceExplorer(s);
	    } else if (s.get(State.READY))
		reinforceDefense(s);
	    
	    MoveUtils.moveSiteToSite(s, s.target());
	}

	Collections.sort(interior, minUnitsCompare);
	for (Site s : interior) {
	    if (s.get(State.READY)) {
		Direction bump = null;
		if (s.damage != 0) {
		    for (Direction d : Site.CARDINALS) {
			Site neighbor = s.neighbors.get(d);
			if (MoveUtils.validMove(s, neighbor) && (s.damage < neighbor.damage) // &&
			    // (!s.moving() || (((s.units == 255) && (MoveUtils.totalUnits(s, s.target()) > MoveUtils.totalUnits(s, neighbor))) ||
			    // 		     ((s.units != 255) && (MoveUtils.totalUnits(s, s.target()) < MoveUtils.totalUnits(s, neighbor))
			    // 		      )))
			    ) {
			    s.heading = d;
			    bump = null;
			}

			if (MoveUtils.validBump(s, neighbor) && (s.damage < neighbor.damage) &&
			    ((s.moving() && s.target().damage < neighbor.damage) ||
			     !s.moving())) {
			    s.heading = d;
			    bump = d;
			}
		    }
		} else {
		    for (Direction d : Site.CARDINALS) {
			Site neighbor = s.neighbors.get(d);		
			if (MoveUtils.validMove(s, neighbor) && (s.reinforce < neighbor.reinforce) // &&
			    // (!s.moving() || (((s.units == 255) && (MoveUtils.totalUnits(s, s.target()) > MoveUtils.totalUnits(s, neighbor))) ||
			    // 		     ((s.units != 255) && (MoveUtils.totalUnits(s, s.target()) < MoveUtils.totalUnits(s, neighbor))
			    // 		      )))
			    ) {
			    s.heading = d;
			    bump = null;
			}

			if (MoveUtils.validBump(s, neighbor) && (s.reinforce < neighbor.reinforce) && 
			    ((s.moving() && (s.target().reinforce < neighbor.reinforce)) ||
			     !s.moving())) {
			    s.heading = d;
			    bump = d;
			}
		    }
		}

		if (bump != null) {
		    Site target = s.target();
		    target.heading = bump;
		    MoveUtils.moveSiteToSite(target, s);
		}
		
		MoveUtils.moveSiteToSite(s, s.target());
	    }
	}
    }
}
