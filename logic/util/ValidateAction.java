package logic.util;

import java.util.HashSet;
import java.util.function.Predicate;

import game.GameMap;
import game.Site;

import logic.Constants;
import logic.Constants.P;
import logic.Constants.S;

public class ValidateAction {
    public static boolean bump(Site a, Site b) {
	if (a.isNot(S.USED) &&
	    b.is(S.MINE) &&
	    !b.moving() &&
	    b.isNot(S.USED) &&
	    (a.units > b.units) &&
	    (b.v(P.ALLOWED_UNITS) >= a.units) &&
	    (a.v(P.ALLOWED_UNITS) >= b.units)) {
	    
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
	if (a.isNot(S.USED) && b.is(S.MINE) && (b.v(P.ALLOWED_UNITS) >= a.units)) {
	    float units = a.units + b.incoming + b.units - b.outgoing;
	    return units <= Constants.MAX_UNITS;
	}
	return false;
    }

    public static boolean explore(Site a, Site b) {
	if (a.isNot(S.USED) && b.is(S.UNEXPLORED) && (b.v(P.ALLOWED_UNITS) >= a.units) && (b.incoming < b.units)) {
	    float remainingUnits = a.units + b.incoming - b.units;
	    return (remainingUnits > 0) && (remainingUnits <= Constants.MAX_UNITS);
	}
	return false;
    }
    
    public static boolean claim(Site a, Site b) {
	if (a.isNot(S.USED) && b.is(S.UNEXPLORED) && b.is(S.OBJECTIVE) && (b.v(P.ALLOWED_UNITS) >= a.units) && (b.incoming < b.units)) {
	    float remainingUnits = a.units + b.incoming - b.units;
	    return (remainingUnits > 0) && (remainingUnits <= Constants.MAX_UNITS);
	}
	return false;
    }
    
    public static boolean capture(Site a, Site b) {
	return a.isNot(S.USED) && b.is(S.OPEN) && (b.v(P.ALLOWED_UNITS) >= a.units) && (b.incoming == 0);
    }

    public static boolean breach(Site a, Site b, GameMap map) {
	if (a.isNot(S.USED) && b.is(S.NEUTRAL) && b.is(S.GATE) && (b.v(P.ALLOWED_UNITS) >= a.units)) {
	    float highestUnit = 0;
	    boolean strongerThan = true;

	    float totalUnits = 0f;
	    RingIterator ri = new RingIterator(a,
					       new Predicate<Site>() {
						   @Override
						   public boolean test(Site t) {
						       return t.is(S.MINE);
						   }
					       });
	    for (int i = 0; i < 3 && ri.hasNext(); i++)
		for (Site s : ri.next())
		    totalUnits += s.units;
	    
	    for (Site neighbor : b.neighbors.values()) {
		float v = neighbor.v(P.GENERATOR);
		if (neighbor.is(S.ENEMY) && (highestUnit < v)) {
		    highestUnit = v;
		    if (map.getEnemy(neighbor.owner).totalUnits > (0.95 * map.bot.totalUnits))
			strongerThan = false;
		}
	    }
 	    if (!strongerThan && (totalUnits < 1650))
	    	return false;
	    highestUnit = highestUnit > b.units ? highestUnit : b.units;
	    float v = a.units + b.incoming - highestUnit;
	    return (v > 0) && (v <= Constants.MAX_UNITS);
	} else
	    return false;
    }
    
    public static boolean attack(Site a, Site b) {
	if (a.isNot(S.USED) && b.is(S.NEUTRAL) && b.is(S.BATTLE) && b.isNot(S.GATE) && b.isNot(S.OPEN)
	    && (b.v(P.ALLOWED_UNITS) >= a.units)) {
	    float v = a.units + b.incoming;
	    return (v <= Constants.MAX_UNITS);
	} else
	    return false;
    }
}
