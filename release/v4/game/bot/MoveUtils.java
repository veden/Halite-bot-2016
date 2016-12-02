package game.bot;

import game.Site;

public class MoveUtils {

    public static boolean moveSiteToSite(Site a, Site b) {
	if (!a.get(Site.State.USED) && (a != b) && (b.neighbors.get(b.heading) != a)) {
	    a.outgoing += a.units;
	    b.incoming += a.units;
	    a.set(Site.State.USED);
	    return true;
	}
	return false;
    }

    public static boolean validMove(Site a, Site b) {
	if (!a.get(Site.State.USED) && b.get(Site.State.MINE))
	    return (a.units + b.incoming + b.units - b.outgoing + b.generator) < Site.MAX_STRENGTH_LOSSY;
	else
	    return false;
    }

    public static boolean validExplore(Site a, Site b) {
	if (!a.get(Site.State.USED) && b.get(Site.State.NEUTRAL) && b.get(Site.State.EXPLORE_CANDIDATE) && (b.units != 0))
	    return ((a.units + b.incoming - b.units) > 0) && ((a.units + b.incoming - b.units) < Site.MAX_STRENGTH_LOSSY);
	else
	    return false;
    }

    public static boolean validCapture(Site a, Site b) {
	if (!a.get(Site.State.USED) && b.get(Site.State.NEUTRAL) && b.get(Site.State.FIELD)) {
	    return ((a.units + b.incoming) > 0) && ((a.units + b.incoming) < Site.MAX_CAPTURE_STRENGTH);
	} else
	    return false;	
    }

    public static boolean validAttack(Site a, Site b) {
	if (!a.get(Site.State.USED) && b.get(Site.State.NEUTRAL) && (b.get(Site.State.BATTLE) || b.get(Site.State.FRONTIER))) {
	    float highestUnit = 0;
	    for (Site neighbor : b.neighbors.values()) {
		float v = neighbor.generator;
		if (neighbor.get(Site.State.ENEMY) && (highestUnit < v))
		    highestUnit = v;
	    }
	    if (b.units != 0)
		highestUnit = highestUnit > b.units ? highestUnit : b.units;
	    float v = a.units + b.incoming - highestUnit; 
	    return (v > 0) && (v < Site.MAX_STRENGTH_LOSSY);
	} else
	    return false;
	    
    }

    public static boolean validJoint(Site center, boolean explore) {
	float total = 0f;
	for (Site neighbor : center.neighbors.values())
	    if (neighbor.get(Site.State.MINE) && !neighbor.get(Site.State.USED) && neighbor.aboveCombatThreshold() &&
		((explore && neighbor.defense == 0) || !explore))
		total += neighbor.units;
	float v = total + center.incoming - center.units;
	return (v > center.units) && (v < Site.MAX_STRENGTH_LOSSY);
    }
}
