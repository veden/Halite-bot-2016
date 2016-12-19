package bot.util;

import game.Site;
import game.Site.State;

public class MoveUtils {

    public static boolean moveSiteToSite(Site a, Site b) {
	if (!a.get(State.USED) && (a != b)) {
	    a.outgoing += a.units;
	    b.incoming += a.units;
	    a.set(State.USED);
	    return true;
	}
	return false;
    }

    public static float totalDeath(Site a) {
	float death = 0;
	for (Site nn : a.neighbors.values())
	    if (nn.get(State.ENEMY))
	        death += nn.units;
	return death;
    }
    
    public static int totalUnits(Site a, Site b) {
	if (a == b)
	    return a.incoming + a.units - a.outgoing;
	return b.incoming + b.units + a.units - b.outgoing;
    }

    public static boolean validBump(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.MINE) && (!b.moving()) && (a.units > b.units * 1.3)) {
	    float v = a.units + b.units + b.incoming;
	    if (v <= Site.MAX_STRENGTH)
		return false;
	    else
		return v - b.units <= Site.MAX_STRENGTH;
	}
	return false;
    }
    
    public static boolean validMove(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.MINE)) {
	    float units = a.units + b.incoming + b.units - b.outgoing;
	    if (!b.moving())
		units += b.generator;
	    return units <= Site.MAX_STRENGTH_LOSSY;
	}
	return false;
    }

    public static boolean validExplore(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.NEUTRAL) && (b.units != 0)) {
	    float remainingUnits = (a.units + b.incoming - b.units);
	    return (remainingUnits > 0) && (remainingUnits < Site.MAX_STRENGTH);
	}
	return false;
    }

    public static boolean validCapture(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.NEUTRAL) && b.get(State.FIELD) && (b.incoming == 0) && (b.units == 0))
	    return a.units > 0;
	return false;	
    }

    public static boolean validAttack(Site a, Site b) {
	if (!a.get(State.USED) && b.get(State.NEUTRAL) && b.get(State.BATTLE)) {
	    float highestUnit = 0;
	    if (b.units != 0) {
		for (Site neighbor : b.neighbors.values()) {
		    float v = neighbor.generator;
		    if (neighbor.get(State.ENEMY) && (highestUnit < v))
			highestUnit = v;
		}
		highestUnit = highestUnit > b.units ? highestUnit : b.units;
	    }
	    float v = a.units + b.incoming - highestUnit;
	    return (v > 0) && (v <= Site.MAX_STRENGTH_LOSSY);
	} else
	    return false;
    }

    public static boolean validJoint(Site center, boolean explore) {
	float total = 0f;
	for (Site neighbor : center.neighbors.values())
	    if (neighbor.get(State.MINE) && !neighbor.get(State.USED) && ((explore && neighbor.damage == 0) || !explore))
		total += neighbor.units;
	float v = total + center.incoming - center.units;
	if (!explore)
	    return (v > center.units) && (v <= Site.MAX_STRENGTH_LOSSY);
	else
	    return (v > center.units) && (v <= Site.MAX_STRENGTH);
    }
}
