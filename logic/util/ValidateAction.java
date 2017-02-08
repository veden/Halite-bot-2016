package logic.util;

import game.GameMap;
import game.Site;

import logic.Constants;
import logic.Constants.P;
import logic.Constants.S;

public class ValidateAction {
    public static boolean bump(Site a, Site b) {
	if (!a.get(S.USED) &&
	    b.get(S.MINE) &&
	    !b.moving() &&
	    !b.get(S.USED) &&
	    (a.units > b.units) &&
	    (b.value(P.ALLOWED_UNITS) >= a.units) &&
	    (a.value(P.ALLOWED_UNITS) >= b.units)) {
	    
	    float aV = a.units + b.units + b.incoming;
	    float bV = b.units + a.incoming;
	    
	    if (aV <= Constants.MAX_UNITS)
		return false;
	    else {
		return ((aV - b.units <= Constants.MAX_UNITS) &&
			(bV <= Constants.MAX_UNITS));
	    }
	}
	return false;
    }
    
    public static boolean move(Site a, Site b) {
	if (!a.get(S.USED) && b.get(S.MINE) && (b.value(P.ALLOWED_UNITS) >= a.units)) {
	    float units = a.units + b.incoming + b.units - b.outgoing;
	    return units <= Constants.MAX_UNITS;
	}
	return false;
    }

    public static boolean explore(Site a, Site b) {
	if (!a.get(S.USED) && b.get(S.UNEXPLORED) && (b.value(P.ALLOWED_UNITS) >= a.units) && (b.incoming < b.units)) {
	    float remainingUnits = a.units + b.incoming - b.units;
	    return (remainingUnits > 0) && (remainingUnits <= Constants.MAX_UNITS);
	}
	return false;
    }

    public static boolean capture(Site a, Site b) {
	return !a.get(S.USED) && b.get(S.OPEN) && (b.value(P.ALLOWED_UNITS) >= a.units) && (b.incoming == 0);
    }

    public static boolean breach(Site a, Site b, GameMap map) {
	if (!a.get(S.USED) && b.get(S.NEUTRAL) && b.get(S.GATE) && (b.value(P.ALLOWED_UNITS) >= a.units)) {
	    float highestUnit = 0;
	    boolean strongerThan = true;
	    for (Site neighbor : b.neighbors.values()) {
		float v = neighbor.value(P.GENERATOR);
		if (neighbor.get(S.ENEMY) && (highestUnit < v)) {
		    highestUnit = v;
		    if (map.getEnemy(neighbor.owner).totalUnits > (0.95 * map.bot.totalUnits))
			strongerThan = false;
		}
	    }
 	    if (!strongerThan)
		return false;
	    highestUnit = highestUnit > b.units ? highestUnit : b.units;
	    float v = a.units + b.incoming - highestUnit;
	    return (v > 0) && (v <= Constants.MAX_UNITS);
	} else
	    return false;
    }
    
    public static boolean attack(Site a, Site b) {
	if (!a.get(S.USED) && b.get(S.NEUTRAL) && b.get(S.BATTLE) && !b.get(S.GATE) && !b.get(S.OPEN) && (b.value(P.ALLOWED_UNITS) >= a.units)) {
	    float v = a.units + b.incoming;
	    return (v <= Constants.MAX_UNITS);
	} else
	    return false;
    }
}
