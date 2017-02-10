package logic.util;

import java.util.ArrayList;

import game.GameMap;
import game.Site;
import game.Stats;

import logic.Constants;
import logic.Constants.A;
import logic.Constants.D;
import logic.Constants.F;
import logic.Constants.P;
import logic.Constants.S;

public class Actions {
    
    public static boolean commitMove(Site a, Site b) {
	if (a.isNot(S.USED) && (a != b)) {
	    a.outgoing += a.units;
	    b.incoming += a.units;

	    //b.accumulate(P.ALLOWED_UNITS, -a.units);

	    //	    if (a.action != A.BUMP) {
	    float decay = ((b.incoming + b.units - b.outgoing) / Constants.MAX_UNITS);
	    for (Site n : b.neighbors.values())
		n.scale(F.DAMAGE, 1f - (0.35f * decay));
	
	    for (Site n : a.neighbors.values())
		n.scale(F.REINFORCE, 1f - (0.0125f * decay));
	    //	    }
	    a.set(S.USED);
	    return true;
	}
	return false;
    }
       
    public static void joint(Site s) {
	ArrayList<D> ambushers = new ArrayList<D>();
	for (D d : Constants.CARDINALS) {
	    Site n = s.neighbors.get(d);
	    if (n.is(S.MINE) && n.isNot(S.USED) && (n.v(F.REINFORCE) <= s.v(F.EXPLORE)) && (n.v(F.DAMAGE) == 0))
		ambushers.add(d);
	}
	if (ambushers.size() > 1) {
	    int setSize = 1 << ambushers.size();
	    float lowest = Float.MAX_VALUE;
	    ArrayList<D> lowestAmbushers = new ArrayList<D>();
	    for (int selection = 1; selection < setSize; selection++) {
		int cursor = selection;
		ArrayList<D> temp = new ArrayList<D>();
		if ((cursor & 1) == 1) 
		    temp.add(ambushers.get(0));
		if ((cursor & 2) == 2)
		    temp.add(ambushers.get(1));
		if ((cursor & 4) == 4)
		    temp.add(ambushers.get(2));
		if ((cursor & 8) == 8)
		    temp.add(ambushers.get(3));
		float tempTotal = 0;
		for (D d : temp)
		    tempTotal += s.neighbors.get(d).units;
		tempTotal += s.incoming - s.units;
		if ((tempTotal > 0) && (tempTotal <= Constants.MAX_UNITS) && (tempTotal < lowest)) {
		    lowestAmbushers = temp;
		    lowest = tempTotal;
		}
	    }
	    for (D d : lowestAmbushers) {
		Site neighbor = s.neighbors.get(d);
		neighbor.heading = Site.reverse(d);
		Actions.commitMove(neighbor, s);
		neighbor.action = A.JOINT;
	    }
	}
    }

    public static void claim(Site s) {
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.claim(s, neighbor) && (s.v(F.REINFORCE) <= neighbor.v(F.EXPLORE))) {
		if (s.target() == s)
		    s.heading = d;
		else { 
		    if (((s.target().v(F.EXPLORE) == neighbor.v(F.EXPLORE)) &&
			 (s.target().v(P.EXPLORE_VALUE) < neighbor.v(P.EXPLORE_VALUE))) ||
			(s.target().v(F.EXPLORE) < neighbor.v(F.EXPLORE)))
			s.heading = d;
		}
	    }
	}
	if (s.moving())
	    s.action = A.CLAIM;
    }
    
    public static void explore(Site s) {
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.explore(s, neighbor) && (s.v(F.REINFORCE) <= neighbor.v(F.EXPLORE))) {
		if (s.target() == s)
		    s.heading = d;
		else { 
		    if (((s.target().v(F.EXPLORE) == neighbor.v(F.EXPLORE)) &&
			 (s.target().v(P.EXPLORE_VALUE) < neighbor.v(P.EXPLORE_VALUE))) ||
			(s.target().v(F.EXPLORE) < neighbor.v(F.EXPLORE)))
			s.heading = d;
		}
	    }
	}
	if (s.moving())
	    s.action = A.EXPLORE;
    }


    public static void assist(Site s) {
	Site target = s;
	ArrayList<D> help = new ArrayList<D>();
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (neighbor.is(S.MINE)) {
		if (!neighbor.is(S.USED) && (s.v(F.REINFORCE) >= neighbor.v(F.REINFORCE)) && (neighbor.v(F.DAMAGE) == 0))
		    help.add(d);
	    } else if (neighbor.is(S.UNEXPLORED)) 
		if ((target.v(F.EXPLORE) <= neighbor.v(F.EXPLORE)) && (s.v(F.REINFORCE) <= neighbor.v(F.EXPLORE)))
		    target = neighbor;
	}
	if ((target != s) && ((s.incoming + s.units + s.v(P.GENERATOR)) < target.units) && (s.outgoing == 0)) {
	    int setSize = 1 << help.size();
	    float lowest = Float.MAX_VALUE;
	    ArrayList<D> lowestHelp = new ArrayList<D>();
	    for (int selection = 1; selection < setSize; selection++) {
		int cursor = selection;
		ArrayList<D> temp = new ArrayList<D>();
		if ((cursor & 1) == 1) 
		    temp.add(help.get(0));
		if ((cursor & 2) == 2)
		    temp.add(help.get(1));
		if ((cursor & 4) == 4)
		    temp.add(help.get(2));
		if ((cursor & 8) == 8)
		    temp.add(help.get(3));
		float tempTotal = 0;
		for (D d : temp)
		    tempTotal += s.neighbors.get(d).units;
		tempTotal += s.incoming + s.units + s.v(P.GENERATOR) - target.units;
		if ((tempTotal > 0) && (tempTotal <= Constants.MAX_UNITS) && (tempTotal < lowest)) {
		    lowestHelp = temp;
		    lowest = tempTotal;
		}
	    }
	    for (D d : lowestHelp) {
		Site neighbor = s.neighbors.get(d);
		neighbor.heading = Site.reverse(d);
		Actions.commitMove(neighbor, s);
		neighbor.action = A.ASSIST;
	    }
	    if (lowestHelp.size() > 0)
		s.set(S.USED);
	}
    }
    
    public static void reinforce(Site s, F field) {
	D bump = null;
	
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.move(s, neighbor) && (s.target().v(field) < neighbor.v(field))) {
		s.heading = d;
		bump = null;
	    }
	    
	    if (ValidateAction.bump(s, neighbor) && (s.target().v(field) < neighbor.v(field))) {
		s.heading = d;
		bump = d;
	    }
	}

	if (bump != null) {
	    Site target = s.target();
	    target.heading = Site.reverse(bump);
	    commitMove(target, s);
	    target.action = A.BUMP;
	}
	if (s.moving())
	    s.action = A.REINFORCE;
    }

    public static void attack(Site s) {
	int lowestCount = Integer.MAX_VALUE;
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    int count = 0;
	    for (Site n : neighbor.neighbors.values())
		if (n.is(S.MINE))
		    count++;
		else if (n.is(S.ENEMY))
		    count--;
		else
		    count -= 0.5f;
	    if (ValidateAction.attack(s, neighbor) &&
		(count <= lowestCount) &&
		(s.target().v(F.DAMAGE) <= neighbor.v(F.DAMAGE))) {
		
		lowestCount = count;
		s.heading = d;
	    }
	}
	if (s.moving())
	    s.action = A.ATTACK;
    }

    public static void breach(Site s, GameMap map) {
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.breach(s, neighbor, map) &&
		(s.target().v(F.DAMAGE) <= neighbor.v(F.DAMAGE))) {
		
		s.heading = d;
	    }
	}
	if (s.moving())
	    s.action = A.BREACH;
    }
    
    public static void capture(Site s) {
	int lowestCount = Integer.MAX_VALUE;
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    int count = 0;
	    for (Site n : neighbor.neighbors.values())
		if (n.is(S.MINE))
		    count++;
		else if (n.is(S.ENEMY))
		    count--;
	    if (ValidateAction.capture(s, neighbor) &&
		(count <= lowestCount) &&
		((s.target().v(F.DAMAGE) <= neighbor.v(F.DAMAGE)) || (s.units <= Stats.maxGenerator))) {

		lowestCount = count;
		s.heading = d;
	    }
	}
	if (s.moving())
	    s.action = A.CAPTURE;
    }

    public static void lock(Site s, float mapScaling) {
	if (s.moving() && (s.units > Stats.maxGenerator)) {
	    float unitBuildUp = 0;
	    for (Site n : s.target().neighbors.values())
		if (n.is(S.ENEMY) && (n.units > unitBuildUp))
		    unitBuildUp += n.units;
	    float units = s.units + s.incoming - s.outgoing;
	    if (unitBuildUp < units * 1.16f)
		for (Site n : s.target().neighbors.values())
		    n.set(P.ALLOWED_UNITS, 0);
	}
    }
}
