package logic;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.function.Predicate;

import game.GameMap;
import game.Site;
import game.Site.P;
import game.Site.State;

import logic.model.Entity;
import logic.util.Actions;
import logic.util.CompareUtil;
import logic.util.RingIterator;

public class AI extends Entity {
    
    private Predicate<Site> pMineReinforce = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return t.get(State.MINE) && (t.value(P.REINFORCE) != 0) && (t.value(P.DAMAGE) == 0);
	    }
	};
    
    private Comparator<Site> maxReinforceCompare = CompareUtil.maxProperty(P.REINFORCE);
    private Comparator<Site> maxExploreCompare = CompareUtil.maxProperty(P.EXPLORE);
    
    private Comparator<Site> maxUnitsCompare = new Comparator<Site>() {
	    @Override
	    public int compare(Site arg0, Site arg1) {
		float v = arg1.value(P.DAMAGE) - arg0.value(P.DAMAGE);
		if (v == 0) {
		    v = arg1.value(P.REINFORCE) - arg0.value(P.REINFORCE);
		    if (v == 0) {
			v = arg1.units - arg0.units;
			if (v == 0)
			    return arg0.id - arg1.id;
		    }
		}
		return v > 0 ? 1 : -1;
	    }
	};

    public AI(int id, GameMap map) {
	super(id, map);
    }
    
    public void planTroopMovements() {

	Collections.sort(frontier, maxExploreCompare);
	for (Site s : frontier)
	    for (Site n : s.neighbors.values())
		if (n.get(State.MINE) && (s.value(P.EXPLORE) > n.value(P.REINFORCE)) && (n.value(P.DAMAGE) == 0))
		    n.set(P.REINFORCE, s.value(P.EXPLORE));

	RingIterator myRings = new RingIterator(frontier, pMineReinforce);
	float d = 0f;
	while (myRings.hasNext()) {
	    boolean changed = false;
	    ArrayList<Site> currentSites = myRings.next();
	    Collections.sort(currentSites, maxReinforceCompare);
	    d++;
	    for (Site ss : currentSites) {
		float v = ss.value(P.REINFORCE) * (1f - (Parameters.reinforceSpread * d));
		for (Site n : ss.neighbors.values())
		    if (n.get(State.MINE) && (v > n.value(P.REINFORCE))) {
			n.set(P.REINFORCE, v);
			changed = true;
		    }
	    }
	    if (!changed)
		break;
	}
    }
        
    public void move() {
	ArrayList<Site> warfare = new ArrayList<Site>(battles.size() + gates.size());
	warfare.addAll(battles);
	warfare.addAll(gates);
	
	Collections.sort(warfare, maxUnitsCompare);
	for (Site s : warfare) {
	    Actions.capture(s);
	    if (!s.moving()) {
		Actions.attack(s);
		if (s.moving())
		    Actions.lock(s, map.scaling);
	    }
	    if (!s.moving() && s.get(State.COMBAT_READY))
		Actions.reinforce(s, P.DAMAGE);

	    if (s.get(State.GATE) && !s.moving())
		Actions.breach(s);
	    
	    Actions.commitMove(s, s.target());
	}

	Collections.sort(frontier, maxExploreCompare);
	float totalExplore = 0f;
	for (Site f : frontier)
	    totalExplore += f.value(P.EXPLORE);
	totalExplore *= 0.75f;
	for (Site s : frontier)
	    if (totalExplore > 0) {
		totalExplore -= s.value(P.EXPLORE);
		Actions.joint(s);
	    } else
		break;

	ArrayList<Site> body = new ArrayList<Site>(border.size() + interior.size());
	body.addAll(border);
	body.addAll(interior);
	
	Collections.sort(body, maxUnitsCompare);	
	for (Site s : body) {
	    if (s.get(State.READY)) {
		if (s.value(P.DAMAGE) == 0) {
		    Actions.reinforce(s, P.REINFORCE);
		} else
		    Actions.reinforce(s, P.DAMAGE);
	    }

	    if (s.get(State.BORDER) && !s.moving())
		Actions.explore(s);
	    
	    Actions.commitMove(s, s.target());
	}
    }
}
