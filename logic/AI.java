package logic;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Random;
import java.util.function.Predicate;

import game.GameMap;
import game.Site;
import game.Site.Direction;
import game.Site.State;
import game.Stats;

import logic.model.Entity;
import logic.util.MoveUtils;
import logic.util.RingIterator;

public class AI extends Entity {

    Random random = new Random(232);
    
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

    private Comparator<Site> maxReinforceCompare = new Comparator<Site>() {
	    @Override
	    public int compare(Site o1, Site o2) {
		float v = o2.reinforce - o1.reinforce;
		if (v == 0)
		    return o1.id - o2.id;
		return v > 0 ? 1 : -1;
	    }
	};
    
    private Comparator<Site> maxUnitsCompare = new Comparator<Site>() {
	    @Override
	    public int compare(Site arg0, Site arg1) {
		float v = arg1.damage - arg0.damage;
		if (v == 0) {
		    v = arg1.reinforce - arg0.reinforce;
		    if (v == 0) {
			v = arg1.units - arg0.units;
			return arg0.id - arg1.id;
		    }
		}
		return v > 0 ? 1 : -1;
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

    private float mapScaling;
    
    public AI(byte id, GameMap map) {
	super(id, map);
    }

    public void spreadDamage(Site s, float defenseRange) {
	float highest = -Float.MAX_VALUE;
	for (Site neighbor : s.neighbors.values())
	    if (neighbor.get(State.NEUTRAL) && (neighbor.get(State.BATTLE) || neighbor.get(State.GATE)) && (neighbor.damage > highest) && (neighbor.damage != 0))
		highest = neighbor.damage;
	if (highest != -Float.MAX_VALUE) {
	    float sv = 0.90f * highest;
	    if (sv > s.damage)
		s.damage = sv;
	    RingIterator ri = new RingIterator(s, pMine);
	    for (int d = 0; (d < defenseRange) && ri.hasNext(); d++) {
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
    
    public void planTroopMovements() {
	mapScaling = (1 - ((float)map.unexplored.size() / Stats.totalSites));
	float defenseRange = (mapScaling * map.scaler) * (0.05f + (0.075f * (map.scaler / GameMap.MAX_SIZE)));
	
	for (Site s : battles)
	    spreadDamage(s, defenseRange);
	for (Site s : gates)
	    spreadDamage(s, defenseRange);
    }

    private boolean captureObjective(Site s) {
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (neighbor.get(State.OBJECTIVE) && MoveUtils.validExplore(s, neighbor, false) &&
		(s.target().explore < neighbor.explore))
		s.heading = d;
	}
	return s.moving();
    }

    private boolean reinforceDefense(Site s) {
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (MoveUtils.validMove(s, neighbor) && (s.target().damage < neighbor.damage))
		s.heading = d;
	}
	return s.moving();
    }

    private boolean exploreSite(Site s) {
	//	float highExplore = 0;
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    // if (neighbor.explore > highExplore)
	    // 	highExplore = neighbor.explore;
	    if (MoveUtils.validExplore(s, neighbor, false) && (s.target().explore < neighbor.explore))
		s.heading = d;
	}
	// if (highExplore != s.target().explore)
	//     s.heading = Direction.STILL;
	return s.moving();
    }

    private boolean reinforceExplorer(Site s) {
	Direction bump = null;
	
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (MoveUtils.validMove(s, neighbor) && (s.target().reinforce < neighbor.reinforce)) {
		s.heading = d;
		bump = null;
	    }

	    if (MoveUtils.validBump(s, neighbor) && (s.reinforce < neighbor.reinforce) && (s.target().reinforce < neighbor.reinforce)) {
		s.heading = d;
		bump = d;
	    }
	}

	if (bump != null) {
	    Site target = s.target();
	    target.heading = Site.reverse(bump);
	    MoveUtils.moveSiteToSite(target, s);
	}
	
	return s.moving();
    }

    private boolean attackSite(Site s) {
	int lowestCount = Integer.MAX_VALUE;
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    int count = 0;
	    for (Site n : neighbor.neighbors.values())
		if (n.get(State.MINE))
		    count++;
		else if (n.get(State.ENEMY))
		    count--;
	    if (MoveUtils.validAttack(s, neighbor) && (s.damage < neighbor.damage) && (count <= lowestCount) && (s.target().damage <= neighbor.damage)) {
		lowestCount = count;
		s.heading = d;
	    }
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

    private void evade(Site s) {
	if (s.moving() && (s.units > 20) && (mapScaling > 0.12f)) {
	    HashSet<Site> neighborSet = new HashSet<Site>();
	    for (Site n : s.target().neighbors.values())
		n.set(State.LOCKED);
	}
    }
        
    public void move() {
	Collections.sort(battles, maxUnitsCompare);
	for (Site s : battles) {
	    if (!captureSite(s) && s.get(State.COMBAT_READY)) {
		if (!attackSite(s))
		    reinforceDefense(s);
	    }
	    evade(s);
	    
	    MoveUtils.moveSiteToSite(s, s.target());
	}

	Collections.sort(gates, maxUnitsCompare);
	for (Site s : gates) {
	    if (!captureSite(s) && s.get(State.COMBAT_READY)) {
		if (!attackSite(s))
		    reinforceDefense(s);
	    }
	    evade(s);
	    
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
			//evade(s);
		    }
		}
	}
	
	Collections.sort(border, maxUnitsCompare);	
	for (Site s : border) {
	    if (s.damage == 0) {
		if (!captureObjective(s)) 
		    if (s.get(State.READY))
			reinforceExplorer(s);
		if (!s.moving())
		    exploreSite(s);
	    } else if (s.get(State.READY)) {
		if (!reinforceDefense(s))
		    exploreSite(s);
	    } 
	    
	    MoveUtils.moveSiteToSite(s, s.target());
	}

	Collections.sort(interior, maxUnitsCompare);
	for (Site s : interior) {
	    if (!s.get(State.USED) && s.get(State.READY)) {
		Direction bump = null;
		if (s.damage != 0) {
		    for (Direction d : Site.CARDINALS) {
			Site neighbor = s.neighbors.get(d);
			if (MoveUtils.validMove(s, neighbor) && (s.damage < neighbor.damage)) {
			    s.heading = d;
			    bump = null;
			}

			if (MoveUtils.validBump(s, neighbor) && (s.damage < neighbor.damage) && (s.target().damage < neighbor.damage)) {
			    s.heading = d;
			    bump = d;
			}
		    }
		} else {
		    for (Direction d : Site.CARDINALS) {
			Site neighbor = s.neighbors.get(d);		
			if (MoveUtils.validMove(s, neighbor) && (s.reinforce < neighbor.reinforce)) {
			    s.heading = d;
			    bump = null;
			}

			if (MoveUtils.validBump(s, neighbor) && (s.reinforce < neighbor.reinforce) && (s.target().reinforce < neighbor.reinforce)) {
			    s.heading = d;
			    bump = d;
			}
		    }
		}

		if (bump != null) {
		    Site target = s.target();
		    target.heading = Site.reverse(bump);
		    MoveUtils.moveSiteToSite(target, s);
		} 
		
		MoveUtils.moveSiteToSite(s, s.target());
	    }
	}
    }
}
