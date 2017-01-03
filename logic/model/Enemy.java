package logic.model;

import java.util.Collections;
import java.util.Comparator;
import java.util.function.Predicate;

import game.GameMap;
import game.Site;
import game.Site.State;
import game.Stats;

import logic.util.RingIterator;

public class Enemy extends Entity {
    public Comparator<Site> maxDamage;

    public Enemy(int id, GameMap map) {
	super(id, map);
    }

    public void spreadDamage(Site s, Predicate<Site> p, boolean isGate) {
	float svalue;
	svalue = 1f + (0.2f * (s.generator / Stats.maxGenerator)) + (0.1f * (s.units / Site.MAX_STRENGTH));
	if (svalue > s.damage)
	    s.damage = svalue;
	for (Site neighbor : s.neighbors.values()) {
	    if ((neighbor.get(State.BATTLE) || neighbor.get(State.GATE)) && neighbor.get(State.NEUTRAL)) {
		float nvalue; 
		if (isGate || neighbor.get(State.GATE))
		    nvalue = 0.45f * s.damage;
		else
		    nvalue = 0.9f * s.damage;
		if (nvalue > neighbor.damage)
		    neighbor.damage = nvalue;
		if (neighbor.get(State.BATTLE)) {
		    RingIterator ri = new RingIterator(neighbor, p);
		    for (int d = 0; d < 8 && ri.hasNext(); d++) 
			for (Site r : ri.next()) {
			    for (Site n : r.neighbors.values()) {
				float v;
				if (isGate || (neighbor.get(State.GATE)))
				    v = 0.45f * n.damage;
				else
				    v = 0.9f * n.damage;
				if (v > r.damage)
				    r.damage = v;
			    }
			}
		}
	    }
	}
    }

    public void placeDefense() {
	Comparator<Site> maxGeneratorUnits = new Comparator<Site>() {
		@Override
		public int compare(Site arg0, Site arg1) {
		    float v = arg1.generator - arg0.generator;
		    if (v == 0) {
			v = arg1.units - arg0.units;
			if (v == 0)
			    return arg0.id - arg1.id;
		    }	
		    return v > 0 ? 1 : -1;
		}
	    };
	
	
	Predicate<Site> np = new Predicate<Site>() {
		@Override
		public boolean test(Site t) {
		    return ((t.get(State.NEUTRAL) && (t.get(State.BATTLE) ||
						      t.get(State.GATE))) ||
			    t.get(State.MINE));
		}
	    };

	Collections.sort(interior, maxGeneratorUnits);
	for (Site i : interior)
	    i.damage = 1f + (0.2f * (i.generator / Stats.maxGenerator));
	Collections.sort(border, maxGeneratorUnits);
	for (Site b : border)
	    b.damage = 1f + (0.2f * (b.generator / Stats.maxGenerator));
	Collections.sort(interior, maxGeneratorUnits);
	for (Site g : gates)
	    spreadDamage(g, np, true);
	Collections.sort(interior, maxGeneratorUnits);
	for (Site c : battles)
	    spreadDamage(c, np, false);
    }
}
