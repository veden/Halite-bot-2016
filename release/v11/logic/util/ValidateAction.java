package logic.util;

import game.Site;
import game.Site.State;

public class ValidateAction {
    public static float totalGenerator(Site a) {
	float generator = (float)a.generator;
	for (Site nn : a.neighbors.values())
	    if (nn.get(State.ENEMY))
	        generator += (float)nn.generator;
	return generator;
    }
    
    public static float totalDeath(Site a) {
	float death = 0f;
	for (Site nn : a.neighbors.values())
	    if (nn.get(State.ENEMY))
	        death += (float)nn.units;
	return death;
    }
    
    public static int totalUnits(Site a, Site b) {
	if (a == b)
	    return a.incoming + a.units - a.outgoing;
	return b.incoming + b.units + a.units - b.outgoing;
    }

    public static boolean bump(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.MINE) && (!b.moving()) && (a.units > b.units * 1.3) && (!b.get(State.LOCKED))) {
	    float v = a.units + b.units + b.incoming;
	    if (v <= Site.MAX_STRENGTH)
		return false;
	    else
		return v - b.units <= Site.MAX_STRENGTH;
	}
	return false;
    }
    
    public static boolean move(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.MINE) && (!b.get(State.LOCKED))) {
	    float units = a.units + b.incoming + b.units - b.outgoing;
	    if (!b.moving())
		units += b.generator;
	    return units <= Site.MAX_STRENGTH;
	}
	return false;
    }

    public static boolean explore(Site a, Site b, boolean joint) {
	if (!a.get(State.USED) && b.get(State.UNEXPLORED) && (!b.get(State.LOCKED))) {
	    float remainingUnits = a.units + b.incoming;
	    float cap = Site.MAX_STRENGTH;
	    if (joint)
		cap += b.units;
	    else
		remainingUnits -= b.units;
	    return (remainingUnits > 0) && (remainingUnits < cap);
	}
	return false;
    }

    public static boolean capture(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.OPEN) && (!b.get(State.LOCKED))) {
	    float units = (a.units + b.incoming);
	    return (units > 0) && (units < Site.MAX_STRENGTH);
	}
	return false;	
    }

    public static boolean breach(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.NEUTRAL) && b.get(State.GATE) && (!b.get(State.LOCKED))) {
	    float highestUnit = 0;
	    for (Site neighbor : b.neighbors.values()) {
		float v = neighbor.generator;
		if (neighbor.get(State.ENEMY) && (highestUnit < v))
		    highestUnit = v;
	    }
	    highestUnit = highestUnit > b.units ? highestUnit : b.units;
	    float v = a.units + b.incoming - highestUnit;
	    return (v > 0) && (v <= Site.MAX_STRENGTH);
	} else
	    return false;
    }
    
    public static boolean attack(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.NEUTRAL) && b.get(State.BATTLE) && (!b.get(State.LOCKED))) {
	    float highestUnit = 0;
	    for (Site neighbor : b.neighbors.values()) {
		float v = neighbor.generator;
		if (neighbor.get(State.ENEMY) && (highestUnit < v))
		    highestUnit = v;
	    }
	    float v = a.units + b.incoming - highestUnit;
	    return (v > 0) && (v <= Site.MAX_STRENGTH);
	} else
	    return false;
    }

    public static boolean joint(Site center, boolean explore) {
	if (center.get(State.UNEXPLORED) && !center.get(State.LOCKED)) {
	    float total = 0f;
	    for (Site neighbor : center.neighbors.values())
		if (neighbor.get(State.MINE) && !neighbor.get(State.USED) && (neighbor.reinforce <= center.explore) && ((explore && neighbor.damage == 0) || !explore))
		    total += neighbor.units;
	    float v = total + center.incoming - center.units;
	    if (!explore)
		return (v > center.units) && (v <= Site.MAX_STRENGTH);
	    else
		return (v > center.units) && (v <= Site.MAX_STRENGTH);
	}
	return false;
    }
}
