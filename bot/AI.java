package bot;

import java.util.Collections;
import java.util.Comparator;
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

    // private Predicate<Site> pMaxUnits = new Predicate<Site>() {
    // 	    @Override
    // 	    public boolean test(Site t) {
    // 		return (t.units >= 230) && t.get(State.MINE) && !t.moving();
    // 	    }
    // 	};

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
		float v = arg1.units - arg0.units;
		if (v == 0) {
		    v = arg1.damage - arg0.damage;
		    if (v == 0) {
			v = arg1.reinforce - arg0.reinforce;

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
	float mapScaling = (1 - ((float)map.unexplored.size() / Stats.totalSites));
	float defenseRange = (map.scaler * mapScaling) * 0.50f;
	boolean allIn = false;
	if (mapScaling >= 0.85f)
	    allIn = true;
	
	for (Site s : battles)
	    spreadDamage(s, allIn, defenseRange);
	for (Site s : gates)
	    spreadDamage(s, allIn, defenseRange);

	if (!allIn) {
	    Predicate<Site> pFrontier = new Predicate<Site>() {
		    @Override
		    public boolean test(Site s) {
			return !s.get(State.FRONTIER) && s.get(State.UNEXPLORED);
		    }
		};
	    
	    for (Site s : frontier) {
		float totalExplore = 0f;
		RingIterator ri = new RingIterator(s, pFrontier);
		for (int d = 0; d < 2 && ri.hasNext(); d++)
		    for (Site n : ri.next())
			totalExplore += n.explore * (0.3f * (1 + d));
		for (Site n : s.neighbors.values())
		    if (n.get(State.MINE) && (n.damage == 0)) {
			float v = totalExplore;
			if (v > n.reinforce)
			    n.reinforce = v;
		    }
	    }
	
	    Collections.sort(spears, maxReinforceCompare);
	    float spearDiffusion = Math.max(0.95f - mapScaling, 0.5f);
	    for (Site s : spears) {
		RingIterator ri = new RingIterator(s, pMine);
		while (ri.hasNext()) {
		    for (Site r : ri.next()) {
			for (Site n : r.neighbors.values()) {
			    float v = spearDiffusion * n.reinforce;
			    if (v > r.reinforce)
				r.reinforce = v;
			}
		    }
		}
	    }
	
	    Collections.sort(border, maxReinforceCompare);
	    float borderDiffusion = Math.min(0.5f + mapScaling, 0.90f);
	    for (Site s : border) {
		RingIterator ri = new RingIterator(s, pMine);
		while (ri.hasNext()) {
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

    public void finish() {
	// ArrayList<Site> herd = new ArrayList<Site>();
	// for (Site s : battles)
	//     if ((s.units >= 230) && (!s.moving()))
	// 	herd.add(s);    
	// for (Site s : border)
	//     if ((s.units >= 230) && (!s.moving()))
	// 	herd.add(s);
	// for (Site s : spears)
	//     if ((s.units >= 230) && (!s.moving()))
	// 	herd.add(s);
	// for (Site s : herd) {
	//     if (!s.get(State.USED)) {
	// 	RingIterator ri = new RingIterator(s, pMaxUnits);
	// 	HashSet<Site> ring = ri.next();
	// 	if ((ring.size() >= 1) && exploreSite(s))
	// 	    MoveUtils.moveSiteToSite(s, s.target());
	// 	do {
	// 	    for (Site rn : ring)
	// 		if (exploreSite(rn))
	// 		    MoveUtils.moveSiteToSite(rn, rn.target());
	// 	    ring = ri.next();
	//     	} while (ring.size() > 0);
	//     }
	// }
    }
    
    public void move() {
	Collections.sort(battles, maxUnitsCompare);
	
	for (Site s : battles) {
	    if (!captureSite(s) && s.get(State.COMBAT_READY)) {
		float best = -Float.MAX_VALUE;
		for (Direction d : Site.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);
		    if (MoveUtils.validAttack(s, neighbor)) {
			float temp = s.units > 100 ? MoveUtils.totalGenerator(neighbor) : MoveUtils.totalDeath(neighbor);
			if ((s.damage < neighbor.damage) && (best <= temp)) {
			    best = temp;
			    s.heading = d;
			}
		    }
		}
		
		if (s.moving()) {
		    // if (s.units > 20)
		    // 	for (Site n : new RingIterator(s.target()).next())
		    // 	    n.damage = 0.0f;
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
	
	Collections.sort(spears, maxUnitsCompare); 	
	for (Site s : spears) {
	    if (s.damage == 0) {
		if (!captureObjective(s))
		    if (!s.get(State.READY) || !reinforceExplorer(s))
			exploreSite(s);
	    } else if (s.get(State.READY))
		reinforceDefense(s);
	    
	    MoveUtils.moveSiteToSite(s, s.target());
	}
	
	Collections.sort(border, maxUnitsCompare);	
	for (Site s : border) {
	    if (s.damage == 0) {
		if (!captureObjective(s))
		    if (!exploreSite(s) && s.get(State.READY))
			reinforceExplorer(s);
	    } else if (s.get(State.READY))
		reinforceDefense(s);
	    
	    MoveUtils.moveSiteToSite(s, s.target());
	}

	Collections.sort(interior, maxUnitsCompare);
	for (int i = 0; i < 5; i++)
	    for (Site s : interior) {
		if (s.get(State.READY)) {
		    Direction bump = null;
		    if (s.damage != 0) {
			for (Direction d : Site.CARDINALS) {
			    Site neighbor = s.neighbors.get(d);
			    if (MoveUtils.validMove(s, neighbor) && (s.damage < neighbor.damage) &&
				(!s.moving() || (((s.units == 255) && (MoveUtils.totalUnits(s, s.target()) > MoveUtils.totalUnits(s, neighbor))) ||
						 ((s.units != 255) && (MoveUtils.totalUnits(s, s.target()) < MoveUtils.totalUnits(s, neighbor))
						  )))
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
			    if (MoveUtils.validMove(s, neighbor) && (s.reinforce < neighbor.reinforce) &&
				(!s.moving() || (((s.units == 255) && (MoveUtils.totalUnits(s, s.target()) > MoveUtils.totalUnits(s, neighbor))) ||
						 ((s.units != 255) && (MoveUtils.totalUnits(s, s.target()) < MoveUtils.totalUnits(s, neighbor))
						  )))
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
			Site extendedTarget = s.neighbors.get(bump);
			if (MoveUtils.validMove(target, extendedTarget) && (target.units > 150))
			    target.heading = bump;
			else
			    target.heading = Site.reverse(bump);
			MoveUtils.moveSiteToSite(target, s);
		    }
		
		    MoveUtils.moveSiteToSite(s, s.target());
		}
	    }
    }
}
