package game.bot;

import java.util.BitSet;
import java.util.HashSet;
import java.util.Iterator;

import game.GameMap;
import game.Harness;
import game.Site;

public class SiteUtils {

    public static float scoreExplore(Site center, GameMap map) {
        BitSet used = new BitSet();
	HashSet<Site> current = new HashSet<Site>();
	HashSet<Site> next = new HashSet<Site>();
	current.add(center);
	used.set(center.id);
	float totalWeight = 1f;
	float total = center.getExploreValue();
	float distance = 0;
	Iterator<Site> c = current.iterator();
	while (c.hasNext()) {
	    while (c.hasNext()) {
		Site ringSite = c.next();
		for (Site neighbor : ringSite.neighbors.values()) {
		    if (neighbor.get(Site.State.NEUTRAL) && (neighbor.units != 0)) {
			totalWeight += (1 - (0.2f * distance));
			total += neighbor.getExploreValue();
			if ((distance+1<3) && (!used.get(neighbor.id))) {
			    next.add(neighbor);
			    used.set(neighbor.id);
			}
		    }
		}
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

	return total / totalWeight;
    }

    public static float scoreGenerator(Site center, GameMap map) {
        BitSet used = new BitSet();
	HashSet<Site> current = new HashSet<Site>();
	HashSet<Site> next = new HashSet<Site>();
	current.add(center);
	used.set(center.id);
	float totalWeight = 1f;
	float total = (float)center.generator;
	float distance = 0;
	Iterator<Site> c = current.iterator();
	while (c.hasNext()) {
	    while (c.hasNext()) {
		Site ringSite = c.next();
		for (Site neighbor : ringSite.neighbors.values()) {
		    if (neighbor.get(Site.State.ENEMY)) {
			totalWeight += (1 - (0.1f * distance));
			total += neighbor.generator;
			if ((distance+1<3) && (!used.get(neighbor.id))) {
			    next.add(neighbor);
			    used.set(neighbor.id);
			}
		    }
		}
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

	return total / totalWeight;
    }
    
    public static void spreadDamage(Site b) {
	float totalDamage = 0;
	for (Site neighbor : b.neighbors.values())
	    if (neighbor.get(Site.State.ENEMY))
		totalDamage += neighbor.units;
	b.damage += totalDamage;
	for (Site neighbor : b.neighbors.values()) {
	    if ((neighbor.get(Site.State.NEUTRAL) && (neighbor.units == 0)) || neighbor.get(Site.State.ENEMY)) {
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
	//		b.defense *= 0.1f;
	if (b.damage < 0)
	    b.damage = 0;
	for (Site neighbor : b.neighbors.values()) {
	    if ((neighbor.get(Site.State.NEUTRAL) && (neighbor.units == 0)) || neighbor.get(Site.State.ENEMY)) {
		neighbor.damage -= 0.9f * b.units;
		//		neighbor.defense *= 0.5f;
		if (neighbor.damage < 0)
		    neighbor.damage = 0;
		for (Site secondNeighbor : neighbor.neighbors.values()) {
		    if ((secondNeighbor.get(Site.State.NEUTRAL) && (secondNeighbor.units == 0)) || (neighbor.get(Site.State.ENEMY))) {
			secondNeighbor.damage -= 0.8f * b.units;
			//		neighbor.defense *= 0.7f;
			if (secondNeighbor.damage < 0)
			    secondNeighbor.damage = 0;
		    }
		}
	    }
	}
    }

    public static void floodExplore(Site center, boolean reduce) {
	int distance = 0;
        BitSet used = new BitSet();
	HashSet<Site> current = new HashSet<Site>();
        HashSet<Site> next = new HashSet<Site>();

	current.add(center);

	Iterator<Site> c = current.iterator();
	
	while (c.hasNext()) {
	    while (c.hasNext()) {
		Site ringSite = c.next();
		boolean valid = false;
		if (ringSite == center) {
		    valid = true;
		    if (reduce)
			ringSite.explore = 0;
		    else
			ringSite.explore *= 1.2f;
		} else {
		    for (Site neighbor : ringSite.neighbors.values())
			if ((used.get(neighbor.id) || (center == ringSite)))
			    valid = true;
		}
		if (valid) {
		    used.set(ringSite.id);
		    float v;
		    if (reduce) {
			v = distance * Harness.map.enemyDistance;
			if (v < 1) {
			    ringSite.explore *= v;
			    for (Site neighbor : ringSite.neighbors.values())
				if ((neighbor.owner == center.owner) && !used.get(neighbor.id))
				    next.add(neighbor);
			}
		    } else {
			v = (distance * Harness.map.enemyDistance) + 1;
			if (v < 1.2) {
			    ringSite.explore *= v;	
			    for (Site neighbor : ringSite.neighbors.values())
				if ((neighbor.owner == center.owner) && !used.get(neighbor.id))
				    next.add(neighbor);		    
			}
		    }
		} else
		    next.add(ringSite);
	    }
	    if (next.size() > 0) {
		distance++;
		HashSet<Site> temp = current;
		current = next;
		next = temp;
		next.clear();
		c = current.iterator();
	    }
	}
    }

    public static void floodFillDefense(Site center, GameMap map, boolean limited) {
	int distance = 0;
        BitSet used = new BitSet();
	HashSet<Site> current = new HashSet<Site>();
        HashSet<Site> next = new HashSet<Site>();

	current.add(center);

	Iterator<Site> c = current.iterator();
	
	while (c.hasNext()) {
	    while (c.hasNext()) {
		Site ringSite = c.next();
		boolean valid = false;
		float highestDefense;
		if (ringSite == center) {
		    valid = true;
		    highestDefense = ringSite.defense;
		} else {
		    highestDefense = -Float.MAX_VALUE;
		    for (Site neighbor : ringSite.neighbors.values())
			if ((used.get(neighbor.id) || (center == ringSite))) {
			    valid = true;
			    if (highestDefense < neighbor.defense)
				highestDefense = neighbor.defense;
			}
		}
		if (valid) {
		    used.set(ringSite.id);
		    float v;
		    if (!limited)
			v = (1 - (0.04f * distance)) * highestDefense;
		    else
			v = (1 - (map.defenseRange * distance)) * highestDefense;
		    if (v > ringSite.defense)
			ringSite.defense = v;
		    if (!limited || (limited && (v > 0)))
			for (Site neighbor : ringSite.neighbors.values())
			    if ((((neighbor.owner == center.owner) || (limited && (neighbor.get(Site.State.NEUTRAL) && (neighbor.units == 0)))) &&
				 (limited || (!limited && neighbor.get(Site.State.BATTLE)))) &&
				!used.get(neighbor.id))
				next.add(neighbor);
		} else
		    next.add(ringSite);
	    }
	    if (next.size() > 0) {
		distance++;
		HashSet<Site> temp = current;
		current = next;
		next = temp;
		next.clear();
		c = current.iterator();
	    }
	}
    }
    
    public static void floodFillStrength(Site center, GameMap map) {
	int distance = 0;
        BitSet used = new BitSet();
	HashSet<Site> current = new HashSet<Site>();
        HashSet<Site> next = new HashSet<Site>();

	current.add(center);

	Iterator<Site> c = current.iterator();
	
	while (c.hasNext()) {
	    while (c.hasNext()) {
		Site ringSite = c.next();
		boolean valid = false;
		float highestStrength;
		if (ringSite == center) {
		    valid = true;
		    highestStrength = ringSite.strength;
		} else {
		    highestStrength = -Float.MAX_VALUE;
		    for (Site neighbor : ringSite.neighbors.values())
			if ((used.get(neighbor.id) || (center == ringSite))) {
			    valid = true;
			    if (highestStrength < neighbor.strength)
				highestStrength = neighbor.strength;
			}
		}
		if (valid) {
		    used.set(ringSite.id);
		    float v = (1 - (0.04f * distance)) * highestStrength;
		    if (v > ringSite.strength)
			ringSite.strength = v;
		    for (Site neighbor : ringSite.neighbors.values())
			if ((neighbor.owner == center.owner) && !used.get(neighbor.id))
			    next.add(neighbor);
		} else
		    next.add(ringSite);
	    }
	    if (next.size() > 0) {
		distance++;
		HashSet<Site> temp = current;
		current = next;
		next = temp;
		next.clear();
		c = current.iterator();
	    }
	}
    }
}
