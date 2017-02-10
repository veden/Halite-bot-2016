package logic;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.function.Predicate;

import game.GameMap;
import game.Site;
import game.Stats;

import logic.Constants.F;
import logic.Constants.P;
import logic.Constants.S;
import logic.model.Entity;
import logic.util.Actions;
import logic.util.CompareUtil;
import logic.util.RingIterator;

public class AI extends Entity {
    
    private Predicate<Site> pMineReinforce = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return t.is(S.MINE) && (t.v(F.REINFORCE) != 0) && (t.v(F.DAMAGE) == 0);
	    }
	};

    private Predicate<Site> pMineDistance = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return t.is(S.MINE) && (t.v(P.DISTANCE) == 0);
	    }
	};
    
    private Comparator<Site> maxReinforceCompare = CompareUtil.maxField(F.REINFORCE);
    private Comparator<Site> maxExploreCompare = CompareUtil.maxField(F.EXPLORE);
    
    private Comparator<Site> damageCompare = new Comparator<Site>() {
	    @Override
	    public int compare(Site o1, Site o2) {
		float v = o2.v(F.DAMAGE) - o1.v(F.DAMAGE);
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
		    v = o1.v(P.DISTANCE) - o2.v(P.DISTANCE);
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
	ArrayList<Site> frontier = new ArrayList<Site>((int)Stats.totalSites);
	ArrayList<Site> body = new ArrayList<Site>((int)Stats.totalSites);
	for (Site s : map.sites)
	    if (s.is(S.FRONTIER))
		frontier.add(s);
	    else if (s.is(S.MINE))
		if (s.is(S.INTERIOR) || s.is(S.BORDER))
		    body.add(s);
	
	Collections.sort(frontier, maxExploreCompare);
	for (Site s : frontier)
	    for (Site n : s.neighbors.values())
		if (n.is(S.MINE) && (s.v(F.EXPLORE) > n.v(F.REINFORCE)) && (n.v(F.DAMAGE) == 0))
		    n.set(F.REINFORCE, s.v(F.EXPLORE));

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
		float v = ss.v(F.REINFORCE) * (1f - (Parameters.reinforceSpread * d));
		for (Site n : ss.neighbors.values())
		    if (n.is(S.MINE) && (v > n.v(F.REINFORCE))) {
			n.set(F.REINFORCE, v);
			changed = true;
		    }
	    }
	    if (!changed)
		break;
	}
	
	extendFields(body);
    }

    private void extendFields(ArrayList<Site> body) {
	Predicate<Site> pMineNothing = new Predicate<Site>() {
		@Override
		public boolean test(Site t) {
		    return t.is(S.MINE) && (t.v(F.DAMAGE) == 0) && (t.v(F.REINFORCE) == 0);
		}
	    };

	ArrayList<Site> backfill = new ArrayList<Site>(); 
	for (Site s : body) 
	    if ((s.v(F.REINFORCE) == 0) && (s.v(F.DAMAGE) == 0)) {
		float highestDamage = 0;
		float highestReinforce = 0;
		for (Site n : s.neighbors.values()) {
		    if (n.v(F.DAMAGE) > highestDamage) {
			highestDamage = n.v(F.DAMAGE);
			highestReinforce = -Float.MAX_VALUE;
		    }
			
		    if ((highestDamage == -Float.MAX_VALUE) && (n.v(F.REINFORCE) > highestReinforce))
			highestReinforce = n.v(F.REINFORCE);
		}
		if (highestDamage != 0) {
		    s.set(F.DAMAGE, 0.9f * highestDamage);
		    backfill.add(s);
		} else if (highestReinforce != 0) {
		    s.set(F.REINFORCE, 0.9f * highestReinforce);
		    backfill.add(s);	    
		}
	    }
	
	RingIterator backfiller = new RingIterator(backfill, pMineNothing);
	while (backfiller.hasNext())
	    for (Site s : backfiller.next()) {
		float highestDamage = 0;
		float highestReinforce = 0;
		for (Site n : s.neighbors.values()) {
		    if (n.v(F.DAMAGE) > highestDamage) {
			highestDamage = n.v(F.DAMAGE);
			highestReinforce = -Float.MAX_VALUE;
		    }
			
		    if ((highestDamage == -Float.MAX_VALUE) && (n.v(F.REINFORCE) > highestReinforce))
			highestReinforce = n.v(F.REINFORCE);
		}
		if (highestDamage != 0)
		    s.set(F.DAMAGE, highestDamage * 0.9f);
		else if (highestReinforce != 0)
		    s.set(F.REINFORCE, highestReinforce * 0.9f);
	    }
    }

    private void processWarfare(ArrayList<Site> warfare) {
	for (Site s : warfare) {	    
	    if (!s.moving()) {
		Actions.attack(s);
		if (s.moving())
		    Actions.lock(s, map.scaling);
	    }
	    if (!s.moving())
		Actions.capture(s);
	    if (!s.moving() && s.is(S.COMBAT_READY))
		Actions.reinforce(s, F.DAMAGE);

	    if (s.is(S.GATE) && !s.moving())
		Actions.breach(s, map);
	    
	    Actions.commitMove(s, s.target());
	}
    } 
    
    public void move() {
	float totalExplore = 0f;
	ArrayList<Site> attacks = new ArrayList<Site>((int)Stats.totalSites);
	ArrayList<Site> frontier = new ArrayList<Site>((int)Stats.totalSites);
	ArrayList<Site> warfare = new ArrayList<Site>((int)Stats.totalSites);
	ArrayList<Site> body = new ArrayList<Site>((int)Stats.totalSites);
	for (Site s : map.sites)
	    if (s.is(S.FRONTIER)) {
		frontier.add(s);
		totalExplore += s.v(F.EXPLORE);
	    } else if (s.is(S.MINE))
		if (s.is(S.BORDER) || s.is(S.INTERIOR))
		    body.add(s);
		else if (s.is(S.BATTLE) || s.is(S.GATE))
		    if (s.v(P.ENEMY_UNITS) != 0)
			attacks.add(s);
		    else
			warfare.add(s);

	Collections.sort(attacks, damageCompare);
	Collections.sort(warfare, maxUnitDistanceCompare);
	
	processWarfare(attacks);
	processWarfare(warfare);
	
	Collections.sort(frontier, maxExploreCompare);
		    
	totalExplore *= 0.8f;
	for (Site s : frontier)
	    if (totalExplore > 0) {
		totalExplore -= s.v(F.EXPLORE);
		Actions.joint(s);
	    } else
		break;
	
	Collections.sort(body, maxUnitDistanceCompare);
	for (Site s : body) {
	    if (s.is(S.BORDER) && !s.moving() && (s.v(F.DAMAGE) == 0))
	    	Actions.claim(s);
	    
	    if (s.is(S.READY) && !s.moving()) {
		if (s.v(F.DAMAGE) == 0)
		    Actions.reinforce(s, F.REINFORCE);
		else
		    Actions.reinforce(s, F.DAMAGE);
	    }

	    if (s.is(S.BORDER) && !s.moving()) {
		Actions.explore(s);
		if (!s.moving() && (s.v(F.DAMAGE) == 0))
		    Actions.assist(s);
	    }

	    Actions.commitMove(s, s.target());
	}
    }
}
