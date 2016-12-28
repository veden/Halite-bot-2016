package bot;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
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

    public void spreadDamage(Site s, boolean allIn, float defenseRange) {
	float highest = -Float.MAX_VALUE;
	for (Site neighbor : s.neighbors.values())
	    if (neighbor.get(State.NEUTRAL) && (neighbor.get(State.BATTLE) || neighbor.get(State.GATE)) && (neighbor.damage > highest) && (neighbor.damage != 0))
		highest = neighbor.damage;
	if (highest != -Float.MAX_VALUE) {
	    s.damage = 0.95f * highest;
	    RingIterator ri = new RingIterator(s, pMine);
	    for (int d = 0; ((d < defenseRange) || allIn) && ri.hasNext(); d++) {
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
	//	float exploreScaling = mapScaling > 0.15f ? 0.95f : 0.55f;
	boolean allIn = false;
	if (mapScaling >= 0.95f)
	    allIn = true;
	
	for (Site s : battles)
	    spreadDamage(s, allIn, defenseRange);
	for (Site s : gates)
	    spreadDamage(s, allIn, defenseRange);

	if (!allIn) {
	    // Predicate<Site> pFrontier = new Predicate<Site>() {
	    // 	    @Override
	    // 	    public boolean test(Site s) {
	    // 		return !s.get(State.FRONTIER) && s.get(State.UNEXPLORED);
	    // 	    }
	    // 	};

	    // float totalFrontierExplore = 0f;
	    // for (Site s : frontier)
	    // 	totalFrontierExplore += s.explore;
	    // totalFrontierExplore *= exploreScaling;

	    // Collections.sort(frontier, maxExploreCompare);
	    // for (Site s : frontier) {
	    // 	boolean skip = false;
 	    // 	// for (Site n : s.neighbors.values())
	    // 	//     if (n.get(State.MINE) // && (n.damage != 0)
	    // 	// 	) {
	    // 	// 	skip = true;
	    // 	// 	break;
	    // 	//     }
		    
	    // 	if (!skip)
	    // 	    if (totalFrontierExplore > 0) {
	    // 		//if (s.explore > totalFrontierExplore) {
	    // 		//	    		totalFrontierExplore -= s.explore;
	    // 		s.set(State.EXPLORE_CANDIDATE);
	    // 	    }  else
	    // 		break;
	    // }

	    // for (Site s : frontier) {
	    // 	if (s.get(State.EXPLORE_CANDIDATE)) {
	    // 	    // float totalExplore = 0f;
	    // 	    // RingIterator ri = new RingIterator(s, pFrontier);
	    // 	    // for (int d = 0; d < 0 && ri.hasNext(); d++)
	    // 	    // 	for (Site n : ri.next())
	    // 	    // 	totalExplore += n.explore * (0.35f * (1 + d));
	    // 	    for (Site n : s.neighbors.values())
	    // 		if (n.get(State.MINE) && (n.damage == 0))
	    // 		    if (s.explore > n.reinforce)
	    // 			n.reinforce = s.explore;
	    // 	}
	    // }

	    // Collections.sort(border, maxReinforceCompare);
	    // for (Site s : border) {
	    // 	RingIterator ri = new RingIterator(s, pMine);
	    // 	while (ri.hasNext()) {
	    // 	    float scale = 0.98f;
	    // 	    // if (new RingIterator(s, pObjective).next().size() != 0)
	    // 	    // 	scale = 0.95f;
	    // 	    for (Site r : ri.next()) {
	    // 		for (Site n : r.neighbors.values()) {
	    // 		    float v = n.reinforce * (scale - (0.2f * (r.generator / Site.MAX_STRENGTH)));
	    // 		    if (v > r.reinforce)
	    // 			r.reinforce = v;
	    // 		}
	    // 	    }
	    // 	}
	    // }
	}
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
