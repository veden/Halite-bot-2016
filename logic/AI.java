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

    private Predicate<Site> pMineDistance = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return t.get(State.MINE) && (t.value(P.DISTANCE) == 0);
	    }
	};
    
    private Comparator<Site> maxReinforceCompare = CompareUtil.maxProperty(P.REINFORCE);
    private Comparator<Site> maxExploreCompare = CompareUtil.maxProperty(P.EXPLORE);
    
    private Comparator<Site> lowestCompare = new Comparator<Site>() {
	    @Override
	    public int compare(Site o1, Site o2) {
		float v = o2.value(P.DAMAGE) - o1.value(P.DAMAGE);
		if (v == 0) {
		    v = o2.units - o1.units;
		    if (v == 0) 
			return o1.id - o2.id;
		}
		return v > 0 ? 1 : -1;
	    }
	};
    
    private Comparator<Site> maxUnitDistanceCompare = new Comparator<Site>() {
	    @Override
	    public int compare(Site o1, Site o2) {
		float v = o2.units - o1.units;
		if (v == 0) {
		    v = o1.value(P.DISTANCE) - o2.value(P.DISTANCE);
		    if (v == 0) 
			return o1.id - o2.id;
		}
		return v > 0 ? 1 : -1;
	    }
	};

    public AI(int id, GameMap map) {
	super(id, map);
    }
    
    public void planTroopMovements() {

	Predicate<Site> pMineNothing = new Predicate<Site>() {
		@Override
		public boolean test(Site t) {
		    return t.get(State.MINE) && (t.value(P.DAMAGE) == 0) && (t.value(P.REINFORCE) == 0);
		}
	    };
	
	Collections.sort(frontier, maxExploreCompare);
	for (Site s : frontier)
	    for (Site n : s.neighbors.values())
		if (n.get(State.MINE) && (s.value(P.EXPLORE) > n.value(P.REINFORCE)) && (n.value(P.DAMAGE) == 0))
		    n.set(P.REINFORCE, s.value(P.EXPLORE));

	RingIterator distanceRings = new RingIterator(frontier, pMineDistance);
	int distance = 1;
	while (distanceRings.hasNext()) {
	    for (Site s : distanceRings.next())
		s.set(P.DISTANCE, distance);
	    distance++;
	}	
	
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

	ArrayList<Site> backfill = new ArrayList<Site>(); 
	for (Site s : body) 
	    if ((s.value(P.REINFORCE) == 0) && (s.value(P.DAMAGE) == 0)) {
		float highestDamage = 0;
		float highestReinforce = 0;
		for (Site n : s.neighbors.values()) {
		    if (n.value(P.DAMAGE) > highestDamage) {
			highestDamage = n.value(P.DAMAGE);
			highestReinforce = -Float.MAX_VALUE;
		    }
			
		    if ((highestDamage == -Float.MAX_VALUE) && (n.value(P.REINFORCE) > highestReinforce))
			highestReinforce = n.value(P.REINFORCE);
		}
		if (highestDamage != 0) {
		    s.set(P.DAMAGE, 0.9f * highestDamage);
		    backfill.add(s);
		} else if (highestReinforce != 0) {
		    s.set(P.REINFORCE, 0.9f * highestReinforce);
		    backfill.add(s);	    
		}
	    }
	
	RingIterator backfiller = new RingIterator(backfill, pMineNothing);
	while (backfiller.hasNext())
	    for (Site s : backfiller.next()) {
		float highestDamage = 0;
		float highestReinforce = 0;
		for (Site n : s.neighbors.values()) {
		    if (n.value(P.DAMAGE) > highestDamage) {
			highestDamage = n.value(P.DAMAGE);
			highestReinforce = -Float.MAX_VALUE;
		    }
			
		    if ((highestDamage == -Float.MAX_VALUE) && (n.value(P.REINFORCE) > highestReinforce))
			highestReinforce = n.value(P.REINFORCE);
		}
		if (highestDamage != 0)
		    s.set(P.DAMAGE, highestDamage * 0.9f);
		else if (highestReinforce != 0)
		    s.set(P.REINFORCE, highestReinforce * 0.9f);
	    }
    }

    private void processWarfare(boolean firstMove) {
	if (firstMove)
	    Collections.sort(warfare, lowestCompare);
	else
	    Collections.sort(warfare, maxUnitDistanceCompare);
	for (Site s : warfare) {
	    if (!s.get(State.ATTACK) && firstMove)
		continue;
	    else if (s.get(State.ATTACK) && !firstMove)
		continue;
	    
	    if (!s.moving()) {
		Actions.attack(s);
		if (s.moving())
		    Actions.lock(s, map.scaling);
	    }
	    if (!s.moving())
		Actions.capture(s);
	    if (!s.moving() && s.get(State.COMBAT_READY))
		Actions.reinforce(s, P.DAMAGE);

	    if (s.get(State.GATE) && !s.moving())
		Actions.breach(s, map);
	    
	    Actions.commitMove(s, s.target());
	}
    } 
    
    public void move() {	
	processWarfare(true);
	processWarfare(false);
	
	Collections.sort(frontier, maxExploreCompare);
	float totalExplore = 0f;
	for (Site f : frontier)
	    totalExplore += f.value(P.EXPLORE);
	totalExplore *= 0.87f;
	for (Site s : frontier)
	    if (totalExplore > 0) {
		totalExplore -= s.value(P.EXPLORE);
		Actions.joint(s);
	    } else
		break;
	
	Collections.sort(body, maxUnitDistanceCompare);
	for (Site s : body) {
	    if (s.get(State.READY)) {
		if (s.value(P.DAMAGE) == 0) {
		    Actions.reinforce(s, P.REINFORCE);
		} else
		    Actions.reinforce(s, P.DAMAGE);
	    }

	    if (s.get(State.BORDER) && !s.moving()) {
		Actions.explore(s);
		if (!s.moving() && (s.value(P.DAMAGE) == 0))
		    Actions.assist(s);
	    }

	    Actions.commitMove(s, s.target());
	}
    }
}
