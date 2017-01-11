package logic.util;

import game.Site;
import game.Site.P;
import game.Site.State;

import logic.Parameters;

public class ValidateAction {
    public static float totalGenerator(Site a) {
	float gen = (float)a.value(P.GENERATOR);
	for (Site nn : a.neighbors.values())
	    if (nn.get(State.ENEMY))
	        gen += (float)nn.value(P.GENERATOR);
	return gen;
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
	if (!a.get(State.USED) && b.get(State.MINE) && !b.moving() && (a.units > b.units * Parameters.bumpMultiplerThreshold) && (b.value(P.LOCKED) > a.units)) {
	    float v = a.units + b.units + b.incoming;
	    if (v <= Site.MAX_STRENGTH)
		return false;
	    else
		return v - b.units <= Site.MAX_STRENGTH;
	}
	return false;
    }
    
    public static boolean move(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.MINE) && (b.value(P.LOCKED) > a.units)) {
	    float units = a.units + b.incoming + b.units - b.outgoing;
	    // if (!b.moving())
	    // 	units += b.value(P.GENERATOR);
	    return units <= Site.MAX_STRENGTH;
	}
	return false;
    }

    public static boolean explore(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.UNEXPLORED) && (b.value(P.LOCKED) > a.units) && (b.incoming == 0)) {
	    float remainingUnits = a.units - b.units;
	    return (remainingUnits > 0) && (remainingUnits < Site.MAX_STRENGTH);
	}
	return false;
    }

    public static boolean capture(Site a, Site b) {
	return !a.get(State.USED) && b.get(State.OPEN) && (b.value(P.LOCKED) > a.units) && (b.incoming == 0);	
    }

    public static boolean breach(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.NEUTRAL) && b.get(State.GATE) && (b.value(P.LOCKED) > a.units)) {
	    float highestUnit = 0;
	    for (Site neighbor : b.neighbors.values()) {
		float v = neighbor.value(P.GENERATOR);
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
	if (!a.get(State.USED) && b.get(State.NEUTRAL) && b.get(State.BATTLE) && !b.get(State.GATE) && !b.get(State.OPEN) && (b.value(P.LOCKED) > a.units)) {
	    float enemyUnits = 0;
	    for (Site neighbor : b.neighbors.values()) {
	    	float v = neighbor.value(P.GENERATOR);
	    	if (neighbor.get(State.ENEMY) && (enemyUnits < v))
	    	    enemyUnits += v;
	    }
	    enemyUnits /= 2.0;
	    float v = a.units + b.incoming - enemyUnits;
	    return (v <= Site.MAX_STRENGTH);
	} else
	    return false;
    }
}
