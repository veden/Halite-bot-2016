package logic.util;

import java.util.ArrayList;

import game.Site;
import game.Site.Direction;
import game.Site.State;
import game.Stats;

public class Actions {

    public static boolean commitMove(Site a, Site b) {
	if (!a.get(State.USED) && (a != b)) {
	    a.outgoing += a.units;
	    b.incoming += a.units;
	    a.set(State.USED);
	    return true;
	}
	return false;
    }
    
    public static void reinforceDefense(Site s) {
	Direction bump = null;
	
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.move(s, neighbor) && (s.target().damage < neighbor.damage)) {
		s.heading = d;
		bump = null;
	    }

	    if (ValidateAction.bump(s, neighbor) && (s.target().damage < neighbor.damage)) {
		s.heading = d;
		bump = d;
	    }
	}

	if (bump != null) {
	    Site target = s.target();
	    target.heading = Site.reverse(bump);
	    commitMove(target, s);
	}	
    }
    
    public static void joint(Site s) {
	ArrayList<Direction> ambushers = new ArrayList<Direction>();
	for (Direction d : Site.CARDINALS) {
	    Site n = s.neighbors.get(d);
	    if (n.get(State.MINE) && !n.get(State.USED) && (n.explore < s.explore) && (n.reinforce <= s.explore) && (n.damage == 0))
		ambushers.add(d);
	}
	int setSize = 1 << ambushers.size();
	float lowest = Float.MAX_VALUE;
	ArrayList<Direction> lowestAmbushers = new ArrayList<Direction>();
	for (int selection = 1; selection < setSize; selection++) {
	    int cursor = selection;
	    ArrayList<Direction> temp = new ArrayList<Direction>();
	    if ((cursor & 1) == 1) 
		temp.add(ambushers.get(0));
	    if ((cursor & 2) == 1)
		temp.add(ambushers.get(1));
	    if ((cursor & 4) == 1)
		temp.add(ambushers.get(2));
	    if ((cursor & 8) == 1)
		temp.add(ambushers.get(3));
	    float tempTotal = 0;
	    for (Direction d : temp)
		tempTotal += s.neighbors.get(d).units;
	    tempTotal += s.incoming - s.units;
	    if ((tempTotal > 0) && (tempTotal <= Site.MAX_STRENGTH) && (tempTotal < lowest)) {
		lowestAmbushers = temp;
		lowest = tempTotal;
	    }
	}
	for (Direction d : lowestAmbushers) {
	    Site neighbor = s.neighbors.get(d);	
	    neighbor.heading = Site.reverse(d);
	    Actions.commitMove(neighbor, s);
	}
    }
    
    public static void explore(Site s) {
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.explore(s, neighbor, false) && (s.target().explore <= neighbor.explore) && (s.reinforce <= neighbor.explore) && ((s.moving() && (s.target().units > neighbor.units)) ||
																		!s.moving()))
		s.heading = d;
	}	
    }

    public static void reinforceExplorer(Site s) {
	Direction bump = null;
	
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.move(s, neighbor) && (s.target().reinforce <= neighbor.reinforce)) {
		s.heading = d;
		bump = null;
	    }

	    if (ValidateAction.bump(s, neighbor) && (s.target().reinforce <= neighbor.reinforce)) {
		s.heading = d;
		bump = d;
	    }
	}

	if (bump != null) {
	    Site target = s.target();
	    target.heading = Site.reverse(bump);
	    commitMove(target, s);
	} 
    }

    public static void attack(Site s) {
	int lowestCount = Integer.MAX_VALUE;
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    int count = 0;
	    for (Site n : neighbor.neighbors.values())
		if (n.get(State.MINE))
		    count++;
		else if (n.get(State.ENEMY))
		    count--;
	    if (ValidateAction.attack(s, neighbor) && (count <= lowestCount) && (s.target().damage <= neighbor.damage)) {
		lowestCount = count;
		s.heading = d;
	    }
	}
    }

    public static void breach(Site s) {
	int lowestCount = Integer.MAX_VALUE;
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    int count = 0;
	    for (Site n : neighbor.neighbors.values())
		if (n.get(State.MINE))
		    count++;
		else if (n.get(State.ENEMY))
		    count--;
	    if (ValidateAction.breach(s, neighbor) && (count <= lowestCount) && (s.target().damage <= neighbor.damage)) {
		lowestCount = count;
		s.heading = d;
	    }
	}
    }
    
    public static void capture(Site s) {
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.capture(s, neighbor) && ((s.damage <= neighbor.damage) || ((s.units <= Stats.maxGenerator) && (neighbor.incoming == 0))))
		s.heading = d;
	}
    }

    public static void evade(Site s, float mapScaling) {
	if (s.moving() && (s.units > Stats.maxGenerator) && (mapScaling > 0.12f)) {
	    // float unitBuildUp = 0;
	    // for (Site n : s.target().neighbors.values())
	    // 	if (n.get(State.ENEMY))
	    // 	    unitBuildUp += n.units;
	    //	    if ((unitBuildUp * 1.4) > s.units)
	    for (Site n : s.target().neighbors.values()) {
		n.set(State.LOCKED);
		if (n.get(State.ENEMY))
		    for (Site en : n.neighbors.values())
			en.set(State.LOCKED);
	    }
	}
    }
}
