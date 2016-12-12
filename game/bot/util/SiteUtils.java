package game.bot.util;

import java.util.BitSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

import game.GameMap;
import game.Site;

public class SiteUtils {

    public static float scoreWeighted(Site center, GameMap map, SiteFunction sf) {
        BitSet used = new BitSet();
	HashSet<Site> current = new HashSet<Site>();
	HashSet<Site> next = new HashSet<Site>();
	current.add(center);
	used.set(center.id);
	float distance = 0;
	Iterator<Site> c = current.iterator();
	while (c.hasNext()) {
	    while (c.hasNext()) {
		Site ringSite = c.next();
		for (Site neighbor : ringSite.neighbors.values())
		    sf.process(neighbor, used, current, next, center, distance);
		sf.postProcess();
	    }
	    if (next.size()>0) {
		distance++;
		HashSet<Site> t = current;
		current = next;
		next = t;
		c = current.iterator();
		next.clear();
	    }
	}

	return sf.total / sf.totalWeight;
    }
    
    public static void spreadDamage(Site b) {
	float totalDamage = 0;
	for (Site neighbor : b.neighbors.values())
	    if (neighbor.get(Site.State.ENEMY))
		totalDamage += neighbor.units;
	b.damage += totalDamage;
	for (Site neighbor : b.neighbors.values()) {
	    if ((neighbor.get(Site.State.NEUTRAL) && (neighbor.units == 0)) || !neighbor.get(Site.State.NEUTRAL)) {
		neighbor.damage += 0.9f * b.units;
		for (Site secondNeighbor : neighbor.neighbors.values()) {
		    if ((secondNeighbor.get(Site.State.NEUTRAL) && (secondNeighbor.units == 0)) || (neighbor.get(Site.State.ENEMY)))
			secondNeighbor.damage += 0.8f * b.units;
		}
	    }
	}
    }

    public static void reduceDamage(Site b) {
	b.damage -= b.units;
	if (b.damage < 0)
	    b.damage = 0;
	for (Site neighbor : b.neighbors.values()) {
	    if ((neighbor.get(Site.State.NEUTRAL) && (neighbor.units == 0)) || !neighbor.get(Site.State.NEUTRAL)) {
		neighbor.damage -= 0.8f * b.units;
		if (neighbor.damage < 0)
		    neighbor.damage = 0;
		for (Site secondNeighbor : neighbor.neighbors.values()) {
		    secondNeighbor.damage += b.units;
		}
	    }
	}
    }
            
    public static void flood(Site center, GameMap map, FloodFunction ff) {
	int distance = 0;
        BitSet used = new BitSet(map.totalSites);
	HashSet<Site> current = new HashSet<Site>();
        HashSet<Site> next = new HashSet<Site>();

	current.add(center);
	
	Iterator<Site> c = current.iterator();
	
	while (c.hasNext()) {
	    while (c.hasNext()) {
		Site ringSite = c.next();
		boolean valid = false;
		if (ringSite == center)
		    valid = true;
		else {
		    ff.best = -Float.MAX_VALUE;
		    for (Site neighbor : ringSite.neighbors.values())
			if (used.get(neighbor.id)) {
			    valid = true;
			    ff.scan(neighbor, center);
			}
		}
		if (valid) {
		    used.set(ringSite.id);
		    ff.process(ringSite, used, center, distance, current, next);
		} else
		    next.add(ringSite);
	    }
	    if ((next.size() > 0) && !ff.abort) {
		distance++;
		HashSet<Site> temp = current;
		current = next;
		next = temp;
		next.clear();
		c = current.iterator();
	    }
	}
    }

    // public static void flow(Site center, GameMap map, FlowFunction ff) {
    // 	int distance = 0;
    //     HashMap<Site, Float> used = new HashMap<Site, Float>();
    // 	HashSet<Site> current = new HashSet<Site>();
    //     HashSet<Site> next = new HashSet<Site>();

    // 	current.add(center);
	
    // 	Iterator<Site> c = current.iterator();
	
    // 	while (c.hasNext()) {
    // 	    while (c.hasNext()) {
    // 		Site ringSite = c.next();
    // 		boolean valid = false;
    // 		if (ringSite == center)
    // 		    valid = true;
    // 		else {
    // 		    ff.best = -Float.MAX_VALUE;
    // 		    for (Site neighbor : ringSite.neighbors.values())
    // 			if (used.get(neighbor.id)) {
    // 			    valid = true;
    // 			    ff.scan(neighbor, center);
    // 			}
    // 		}
    // 		if (valid) {
    // 		    used.put(ringSite.id, ringSite.units + ff.best);
    // 		    ff.process(ringSite, used, center, distance, current, next);
    // 		} else
    // 		    next.add(ringSite);
    // 	    }
    // 	    if ((next.size() > 0) && !ff.abort) {
    // 		distance++;
    // 		HashSet<Site> temp = current;
    // 		current = next;
    // 		next = temp;
    // 		next.clear();
    // 		c = current.iterator();
    // 	    }
    // 	}
    // }   
}
