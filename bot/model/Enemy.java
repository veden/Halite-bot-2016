package bot.model;

import java.util.function.Predicate;

import bot.util.RingIterator;

import game.GameMap;
import game.Site;
import game.Site.State;
import game.Stats;

public class Enemy extends Entity {
    public Enemy(byte id, GameMap map) {
	super(id, map);
    }

    public void spreadDamage(Site s, Predicate<Site> p, boolean isGate) {
	if (s.get(State.GATE))
	    s.damage = 0.45f + (0.2f * ((float)s.generator / Stats.maxGenerator));
	else
	    s.damage = 1f + (0.2f * ((float)s.generator / Stats.maxGenerator));
	for (Site neighbor : s.neighbors.values())
	    if ((neighbor.get(State.BATTLE) || neighbor.get(State.GATE)) && neighbor.get(State.NEUTRAL)) {
		if (isGate || neighbor.get(State.GATE))
		    neighbor.damage = 0.45f * s.damage;
		else
		    neighbor.damage = 0.9f * s.damage;
		if (neighbor.units == 0) {
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
        
    public void placeDefense() {
	Predicate<Site> np = new Predicate<Site>() {
		@Override
		public boolean test(Site t) {
		    return ((t.get(State.NEUTRAL) && (t.get(State.BATTLE) ||
						      t.get(State.GATE))) ||
			    t.get(State.MINE));
		}
	    };

	for (Site i : interior)
	    i.damage = 1f + (0.2f * ((float)i.generator / Stats.maxGenerator));
	for (Site b : border)
	    b.damage = 1f + (0.2f * ((float)b.generator / Stats.maxGenerator));
	for (Site g : gates)
	    spreadDamage(g, np, true);
	for (Site c : battles)
	    spreadDamage(c, np, false);
    }
}
