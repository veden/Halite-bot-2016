package logic.util;

import game.GameMap;
import game.Site;
import game.Site.P;
import game.Site.State;

public class ValidateAction {
    public static boolean bump(Site a, Site b) {
	if (!a.get(State.USED) &&
	    b.get(State.MINE) &&
	    !b.moving() &&
	    !b.get(State.USED) &&
	    (a.units > b.units) &&
	    (b.value(P.LOCKED) >= a.units) &&
	    (a.value(P.LOCKED) >= b.units)) {
	    
	    float aV = a.units + b.units + b.incoming;
	    float bV = b.units + a.incoming;
	    
	    if (aV <= Site.MAX_STRENGTH)
		return false;
	    else {
		return ((aV - b.units <= Site.MAX_STRENGTH) &&
			(bV <= Site.MAX_STRENGTH));
	    }
	}
	return false;
    }
    
    public static boolean move(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.MINE) && (b.value(P.LOCKED) >= a.units)) {
	    float units = a.units + b.incoming + b.units - b.outgoing;
	    return units <= Site.MAX_STRENGTH;
	}
	return false;
    }

    public static boolean explore(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.UNEXPLORED) && (b.value(P.LOCKED) >= a.units) && (b.incoming < b.units)) {
	    float remainingUnits = a.units + b.incoming - b.units;
	    return (remainingUnits > 0) && (remainingUnits <= Site.MAX_STRENGTH);
	}
	return false;
    }

    public static boolean capture(Site a, Site b) {
	return !a.get(State.USED) && b.get(State.OPEN) && (b.value(P.LOCKED) >= a.units) && (b.incoming == 0);
    }

    public static boolean breach(Site a, Site b, GameMap map) {
	if (!a.get(State.USED) && b.get(State.NEUTRAL) && b.get(State.GATE) && (b.value(P.LOCKED) >= a.units)) {
	    float highestUnit = 0;
	    boolean strongerThan = true;
	    for (Site neighbor : b.neighbors.values()) {
		float v = neighbor.value(P.GENERATOR);
		if (neighbor.get(State.ENEMY) && (highestUnit < v)) {
		    highestUnit = v;
		    if (map.getEnemy(neighbor.owner).totalUnits > (0.95 * map.bot.totalUnits))
			strongerThan = false;
		}
	    }
 	    if (!strongerThan)
		return false;
	    highestUnit = highestUnit > b.units ? highestUnit : b.units;
	    float v = a.units + b.incoming - highestUnit;
	    return (v > 0) && (v <= Site.MAX_STRENGTH);
	} else
	    return false;
    }
    
    public static boolean attack(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.NEUTRAL) && b.get(State.BATTLE) && !b.get(State.GATE) && !b.get(State.OPEN) && (b.value(P.LOCKED) >= a.units)) {
	    float v = a.units + b.incoming;
	    return (v <= Site.MAX_STRENGTH);
	} else
	    return false;
    }
}
