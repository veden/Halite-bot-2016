package logic;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.function.Predicate;

import game.GameMap;
import game.Site;
import game.Site.Direction;
import game.Site.State;
import game.Stats;

import logic.model.Entity;
import logic.util.Actions;
import logic.util.RingIterator;
import logic.util.ValidateAction;

public class AI extends Entity {
    
    private Predicate<Site> pMine = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return t.get(State.MINE);
	    }
	};

    private Predicate<Site> pMineReinforce = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return t.get(State.MINE) && (t.reinforce != 0) && (t.damage == 0);
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
    
    public AI(int id, GameMap map) {
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
	float defenseRange = (mapScaling * map.scaler) * (0.05f + (0.05f * (map.scaler / GameMap.MAX_SIZE)));

	Collections.sort(battles, maxUnitsCompare);	
	for (Site s : battles)
	    spreadDamage(s, defenseRange);

	Collections.sort(gates, maxUnitsCompare);
	for (Site s : gates)
	    spreadDamage(s, defenseRange);

	Collections.sort(frontier, maxExploreCompare);
	for (Site s : frontier)
	    for (Site n : s.neighbors.values())
		if (n.get(State.MINE) && (s.explore > n.reinforce) && (n.damage == 0))
		    n.reinforce = s.explore;

	RingIterator myRings = new RingIterator(frontier, pMineReinforce);
	float d = 0f;
	while (myRings.hasNext()) {
	    boolean changed = false;
	    ArrayList<Site> currentSites = myRings.next();
	    Collections.sort(currentSites, maxReinforceCompare);
	    d++;
	    for (Site ss : currentSites) {
		float v = ss.reinforce * (1f - (0.1f * d));
		for (Site n : ss.neighbors.values())
		    if (n.get(State.MINE) && (v > n.reinforce)) {
			n.reinforce = v;
			changed = true;
		    }
	    }
	    if (!changed)
		break;
	}
    }
        
    public void move() {
	Collections.sort(battles, maxUnitsCompare);
	for (Site s : battles) {
	    Actions.attack(s);
	    if (!s.moving() && s.get(State.COMBAT_READY))
		Actions.reinforceDefense(s);
	    if (!s.moving())
		Actions.capture(s);
	    Actions.evade(s, mapScaling);
	    
	    Actions.commitMove(s, s.target());
	}

	Collections.sort(gates, maxUnitsCompare);
	for (Site s : gates) {
	    Actions.attack(s);
	    if (!s.moving() && s.get(State.COMBAT_READY))
		Actions.reinforceDefense(s);
	    if (!s.moving())
		Actions.capture(s);
	    if (!s.moving())
		Actions.breach(s);
	    Actions.evade(s, mapScaling);
	    
	    Actions.commitMove(s, s.target());
	}

	Collections.sort(frontier, maxExploreCompare);
	for (Site s : frontier) {
	    if (ValidateAction.joint(s, true))
		for (Direction d : Site.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);	
		    if (neighbor.get(State.MINE) && ValidateAction.explore(neighbor, s, true)) {
			neighbor.heading = Site.reverse(d);
			Actions.commitMove(neighbor, s);
		    }
		}
	}
	
	Collections.sort(border, maxUnitsCompare);	
	for (Site s : border) {
	    if (s.get(State.READY)) {
		if (s.damage == 0)
		    Actions.reinforceExplorer(s);
		else
		    Actions.reinforceDefense(s);
	    }
		    
	    if (!s.moving())
		Actions.explore(s);
	    
	    Actions.commitMove(s, s.target());
	}

	Collections.sort(interior, maxUnitsCompare);
	for (Site s : interior) {
	    if (s.get(State.READY)) {
		if (s.damage != 0)
		    Actions.reinforceDefense(s);
		else
		    Actions.reinforceExplorer(s);
	        
		Actions.commitMove(s, s.target());
	    }
	}
    }
}
