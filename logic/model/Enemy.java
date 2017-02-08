package logic.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.function.Predicate;

import game.GameMap;
import game.Site;
import game.Stats;

import logic.Constants;
import logic.Constants.F;
import logic.Constants.P;
import logic.Constants.S;
import logic.Parameters;
import logic.util.RingIterator;

public class Enemy extends Entity {

    private static Comparator<Site> maxGeneratorUnits = new Comparator<Site>() {
	    @Override
	    public int compare(Site arg0, Site arg1) {
		float v = arg1.v(P.GENERATOR) - arg0.v(P.GENERATOR);
		if (v == 0) {
		    v = arg1.units - arg0.units;
		    if (v == 0)
			return arg0.id - arg1.id;
		}	
		return v > 0 ? 1 : -1;
	    }
	};

    private static Predicate<Site> isBattleOrMine = new Predicate<Site>() {
	    @Override
	    public boolean test(Site t) {
		return (t.is(S.NEUTRAL) &&
			(t.is(S.BATTLE) || t.is(S.GATE))) || (t.is(S.MINE));
	    }
	};
    
    public Enemy(int id, GameMap map) {
	super(id, map);
    }

    private void identifyTargets(Site s) {
	float svalue;
	svalue = 1f + (Parameters.enemyGeneratorWeight * (s.v(P.GENERATOR) / Stats.maxGenerator));
	s.stagingValue += svalue;
	for (Site n : s.neighbors.values()) {
	    float absorb = svalue * (0.98f - (Parameters.enemyGeneratorSpread * (1 - (n.v(P.GENERATOR) / Stats.maxGenerator))));
	    if (n.is(S.ENEMY))
		n.stagingValue += absorb;
	}
    }

    public void spreadDamage(Site s, float defenseRange) {	
	float svalue = 1f + (Parameters.enemyGeneratorWeight * (s.v(P.GENERATOR) / Stats.maxGenerator)) + (Parameters.enemyUnitWeight * (s.units / Constants.MAX_UNITS));
	if (svalue > s.v(F.DAMAGE))
	    s.set(F.DAMAGE, svalue);
	RingIterator ri = new RingIterator(s, isBattleOrMine);
	for (int d = 0; (d < defenseRange) && ri.hasNext(); d++) {
	    for (Site r : ri.next()) {
		for (Site n : r.neighbors.values()) {
		    float v = 0.90f * n.v(F.DAMAGE);
		    if (v > r.v(F.DAMAGE))
			r.set(F.DAMAGE, v);
		}
	    }
	}
    }
    
    public void placeDefense() {
	ArrayList<Site> body = new ArrayList<Site>((int)Stats.totalSites);
	ArrayList<Site> warfare = new ArrayList<Site>((int)Stats.totalSites);
	for (Site s : map.sites)
	    if (s.owner == id) 
		if (s.is(S.BORDER) || s.is(S.INTERIOR))
		    body.add(s);
		else if (s.is(S.BATTLE) || s.is(S.GATE))
		    warfare.add(s);
	
	
	Collections.sort(body, maxGeneratorUnits);
	
	for (Site b : body)
	    identifyTargets(b);
	for (Site b : body)
	    b.commit(F.DAMAGE);

	float defenseRange = Math.max(map.scaling * 1.125f * map.scale, 8f);
	
	Collections.sort(warfare, maxGeneratorUnits);
	for (Site w : warfare)
	    spreadDamage(w, defenseRange);
    }
}
