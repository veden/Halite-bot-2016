package logic.model;

import java.util.Collections;
import java.util.Comparator;
import java.util.function.Predicate;

import game.GameMap;
import game.Site;
import game.Site.P;
import game.Site.State;
import game.Stats;

import logic.Parameters;
import logic.util.RingIterator;

public class Enemy extends Entity {
    
    public Enemy(int id, GameMap map) {
	super(id, map);
    }

    private void identifyTargets(Site s) {
	float svalue;
	svalue = 1f + (Parameters.enemyGeneratorWeight * (s.value(P.GENERATOR) / Stats.maxGenerator));
	s.stagingValue += svalue;
	for (Site n : s.neighbors.values()) {
	    float absorb = svalue * (0.98f - (Parameters.enemyGeneratorSpread * (1 - (n.value(P.GENERATOR) / Stats.maxGenerator))));
	    if (n.get(State.ENEMY))
		n.stagingValue += absorb;
	}
    }

    public void spreadDamage(Site s, Predicate<Site> p, float defenseRange) {	
	float svalue = 1f + (Parameters.enemyGeneratorWeight * (s.value(P.GENERATOR) / Stats.maxGenerator)) + (Parameters.enemyUnitWeight * (s.units / Site.MAX_STRENGTH));
	if (svalue > s.value(P.DAMAGE))
	    s.set(P.DAMAGE, svalue);
	RingIterator ri = new RingIterator(s, p);
	for (int d = 0; (d < defenseRange) && ri.hasNext(); d++) {
	    for (Site r : ri.next()) {
		for (Site n : r.neighbors.values()) {
		    float v = 0.90f * n.value(P.DAMAGE);
		    if (v > r.value(P.DAMAGE))
			r.set(P.DAMAGE, v);
		}
	    }
	}
    }
    
    public void placeDefense() {
	Comparator<Site> maxGeneratorUnits = new Comparator<Site>() {
		@Override
		public int compare(Site arg0, Site arg1) {
		    float v = arg1.value(P.GENERATOR) - arg0.value(P.GENERATOR);
		    if (v == 0) {
			v = arg1.units - arg0.units;
			if (v == 0)
			    return arg0.id - arg1.id;
		    }	
		    return v > 0 ? 1 : -1;
		}
	    };

	Collections.sort(body, maxGeneratorUnits);
	
	for (Site b : body)
	    identifyTargets(b);
	for (Site b : body)
	    b.commit(P.DAMAGE);

	Predicate<Site> np = new Predicate<Site>() {
		@Override
		public boolean test(Site t) {
		    return (t.get(State.NEUTRAL) &&
			    (t.get(State.BATTLE) || t.get(State.GATE))) || (t.get(State.MINE));
		}
	    };

	float defenseRange = Math.max(map.scaling * 1.125f * map.scale, 8f);
	
	Collections.sort(warfare, maxGeneratorUnits);
	for (Site w : warfare)
	    spreadDamage(w, np, defenseRange);
    }
}
