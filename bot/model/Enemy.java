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

    private Predicate<Site> pDamage = new Predicate<Site>() {
	    @Override
	    public boolean test(Site s) {
		return (s.get(State.BATTLE) || s.get(State.ENEMY) || s.get(State.GATE)) && !s.get(State.MINE);
	    }
	};
    
    public void spreadDamage(Site s, Predicate<Site> p) {
	RingIterator ri = new RingIterator(s, p);
	if (s.damage < s.units)
	    s.damage = s.units;
	for (int d = 0; d < 3; d++)
	    for (Site neighbor : ri.next()) {
		float v = s.damage * (0.9f - (0.05f * d));
		if (v > neighbor.damage)
		    neighbor.damage = v;
	    }
    }
    
    public void placeDamageRadius() {
	for (Site b : interior)
	    spreadDamage(b, pDamage);
	for (Site b : border)
	    spreadDamage(b, pDamage);
	for (Site b : battles)
	    spreadDamage(b, pDamage);
    }

    public void placeDefense() {
	Predicate<Site> np = new Predicate<Site>() {
		@Override
		public boolean test(Site t) {
		    return (t.get(State.NEUTRAL) && (t.get(State.BATTLE))) || t.get(State.MINE);
		}
	    };

	for (Site i : interior)
	    i.damage = 1f + (0.05f * ((float)i.generator / Stats.maxGenerator));
	for (Site b : border)
	    b.damage = 1f + (0.05f * ((float)b.generator / Stats.maxGenerator));
	for (Site c : battles) {
	    c.damage = 1f + (0.05f * ((float)c.generator / Stats.maxGenerator));
	    for (Site neighbor : c.neighbors.values())
		if (neighbor.get(State.BATTLE) && neighbor.get(State.NEUTRAL)) {
		    if (neighbor.units == 0)
			neighbor.damage = 0.95f * c.damage;
		    else
			neighbor.damage = 0.65f * c.damage;
		    if (neighbor.units == 0) {
			RingIterator ri = new RingIterator(neighbor, np);
			for (int d = 0; d < 8 && ri.hasNext(); d++) 
			    for (Site r : ri.next()) {
				for (Site n : r.neighbors.values()) {
				    float v = 0.95f * n.damage;
				    if (v > r.damage)
					r.damage = v;
				}
			    }
		    }
		}
	}    
    }
}
