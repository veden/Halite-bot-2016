package logic.util;

import java.util.ArrayList;

import game.GameMap;
import game.Site;
import game.Stats;

import logic.Constants;
import logic.Constants.D;
import logic.Constants.F;
import logic.Constants.P;
import logic.Constants.S;

public class Actions {

    public static enum Action {
	IDLE, EXPLORE, JOINT, ASSIST, REINFORCE, BUMP, CAPTURE, BREACH, ATTACK
    }
    
    public static boolean commitMove(Site a, Site b) {
	if (!a.get(S.USED) && (a != b)) {
	    a.outgoing += a.units;
	    b.incoming += a.units;
	    
	    for (Site n : b.neighbors.values())
		n.set(F.DAMAGE, n.value(F.DAMAGE) * (1f - (0.35f * ((b.incoming + b.units - b.outgoing) / Constants.MAX_UNITS))));
	
	    for (Site n : a.neighbors.values())
		n.set(F.REINFORCE, n.value(F.REINFORCE) * (1f - (0.0125f * ((b.incoming + b.units - b.outgoing) / Constants.MAX_UNITS))));
	    
	    a.set(S.USED);
	    return true;
	}
	return false;
    }
       
    public static void joint(Site s) {
	ArrayList<D> ambushers = new ArrayList<D>();
	for (D d : Constants.CARDINALS) {
	    Site n = s.neighbors.get(d);
	    if (n.get(S.MINE) &&
		!n.get(S.USED) &&
		(n.value(F.REINFORCE) <= s.value(F.EXPLORE)) &&
		(n.value(F.DAMAGE) == 0))
		
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
		neighbor.action = Action.JOINT;
	    }
	}
    }
    
    public static void explore(Site s) {
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.explore(s, neighbor) && (s.value(F.REINFORCE) <= neighbor.value(F.EXPLORE))) {
		if (s.target() == s)
		    s.heading = d;
		else { 
		    if (((s.target().value(F.EXPLORE) == neighbor.value(F.EXPLORE)) &&
			 (s.target().value(P.EXPLORE_VALUE) < neighbor.value(P.EXPLORE_VALUE))) ||
			(s.target().value(F.EXPLORE) < neighbor.value(F.EXPLORE)))
			s.heading = d;
		}
	    }
	}
	if (s.moving())
	    s.action = Action.EXPLORE;
    }


    public static void assist(Site s) {
	Site target = s;
	ArrayList<D> help = new ArrayList<D>();
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (neighbor.get(S.MINE)) {
		if (!neighbor.get(S.USED) && (s.value(F.REINFORCE) >= neighbor.value(F.REINFORCE)) && (neighbor.value(F.DAMAGE) == 0))
		    help.add(d);
	    } else if (neighbor.get(S.UNEXPLORED)) 
		if ((target.value(F.EXPLORE) <= neighbor.value(F.EXPLORE)) && (s.value(F.REINFORCE) <= neighbor.value(F.EXPLORE)))
		    target = neighbor;
	}
	if ((target != s) && ((s.incoming + s.units + s.value(P.GENERATOR)) < target.units) && (s.outgoing == 0)) {
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
		tempTotal += s.incoming + s.units + s.value(P.GENERATOR) - target.units;
		if ((tempTotal > 0) && (tempTotal <= Constants.MAX_UNITS) && (tempTotal < lowest)) {
		    lowestHelp = temp;
		    lowest = tempTotal;
		}
	    }
	    for (D d : lowestHelp) {
		Site neighbor = s.neighbors.get(d);
		neighbor.heading = Site.reverse(d);
		Actions.commitMove(neighbor, s);
		neighbor.action = Action.ASSIST;
	    }
	    if (lowestHelp.size() > 0)
		s.set(S.USED);
	}
    }
    
    public static void reinforce(Site s, F field) {
	D bump = null;
	
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.move(s, neighbor) && (s.target().value(field) < neighbor.value(field))) {
		s.heading = d;
		bump = null;
	    }
	    
	    if (ValidateAction.bump(s, neighbor) && (s.target().value(field) < neighbor.value(field))) {
		s.heading = d;
		bump = d;
	    }
	}

	if (bump != null) {
	    Site target = s.target();
	    target.heading = Site.reverse(bump);
	    commitMove(target, s);
	    target.action = Action.BUMP;
	}
	if (s.moving())
	    s.action = Action.REINFORCE;
    }

    public static void attack(Site s) {
	int lowestCount = Integer.MAX_VALUE;
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    int count = 0;
	    for (Site n : neighbor.neighbors.values())
		if (n.get(S.MINE))
		    count++;
		else if (n.get(S.ENEMY))
		    count--;
		else
		    count -= 0.5f;
	    if (ValidateAction.attack(s, neighbor) &&
		(count <= lowestCount) &&
		(s.target().value(F.DAMAGE) <= neighbor.value(F.DAMAGE))) {
		
		lowestCount = count;
		s.heading = d;
	    }
	}
	if (s.moving())
	    s.action = Action.ATTACK;
    }

    public static void breach(Site s, GameMap map) {
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.breach(s, neighbor, map) &&
		(s.target().value(F.DAMAGE) <= neighbor.value(F.DAMAGE))) {
		
		s.heading = d;
	    }
	}
	if (s.moving())
	    s.action = Action.BREACH;
    }
    
    public static void capture(Site s) {
	int lowestCount = Integer.MAX_VALUE;
	for (D d : Constants.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    int count = 0;
	    for (Site n : neighbor.neighbors.values())
		if (n.get(S.MINE))
		    count++;
		else if (n.get(S.ENEMY))
		    count--;
	    if (ValidateAction.capture(s, neighbor) &&
		(count <= lowestCount) &&
		((s.target().value(F.DAMAGE) <= neighbor.value(F.DAMAGE)) || (s.units <= Stats.maxGenerator))) {

		lowestCount = count;
		s.heading = d;
	    }
	}
	if (s.moving())
	    s.action = Action.CAPTURE;
    }

    public static void lock(Site s, float mapScaling) {
	if (s.moving() && (s.units > Stats.maxGenerator)) {
	    float unitBuildUp = 0;
	    for (Site n : s.target().neighbors.values())
		if (n.get(S.ENEMY) && (n.units > unitBuildUp))
		    unitBuildUp += n.units;
	    float units = s.units + s.incoming - s.outgoing;
	    if (unitBuildUp < units * 1.16f)
		for (Site n : s.target().neighbors.values())
		    n.set(P.ALLOWED_UNITS, 0);
	}
    }
}
