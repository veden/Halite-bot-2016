package logic.util;

import java.util.ArrayList;

import game.Site;
import game.Site.Direction;
import game.Site.P;
import game.Site.State;
import game.Stats;

import logic.Parameters;

public class Actions {

    public static boolean commitMove(Site a, Site b) {
	if (!a.get(State.USED) && (a != b)) {
	    a.outgoing += a.units;
	    b.incoming += a.units;
	    b.set(P.LOCKED, b.value(P.LOCKED) - a.units);
	    for (Site n : b.neighbors.values())
		n.set(P.LOCKED, n.value(P.LOCKED) - a.units);
	    a.set(State.USED);
	    return true;
	}
	return false;
    }
       
    public static void joint(Site s) {
	ArrayList<Direction> ambushers = new ArrayList<Direction>();
	for (Direction d : Site.CARDINALS) {
	    Site n = s.neighbors.get(d);
	    if (n.get(State.MINE) &&
		!n.get(State.USED) &&
		(n.value(P.EXPLORE) <= s.value(P.EXPLORE)) &&
		(n.value(P.REINFORCE) <= s.value(P.EXPLORE)) &&
		(n.value(P.DAMAGE) == 0))
		
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
	    if (ValidateAction.explore(s, neighbor) &&
		(s.target().value(P.EXPLORE) <= neighbor.value(P.EXPLORE)) &&
		(s.value(P.REINFORCE) <= neighbor.value(P.EXPLORE)) &&
		((s.moving() && (s.target().units > neighbor.units)) || !s.moving())) {

		s.heading = d;
	    }
	}	
    }

    public static void reinforce(Site s, P siteProperty) {
	Direction bump = null;
	
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    if (ValidateAction.move(s, neighbor) && (s.target().value(siteProperty) < neighbor.value(siteProperty))) {
		s.heading = d;
		bump = null;
	    }

	    if (ValidateAction.bump(s, neighbor) && (s.target().value(siteProperty) < neighbor.value(siteProperty))) {
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
	    if (ValidateAction.attack(s, neighbor) &&
		(count <= lowestCount) &&
		(s.target().value(P.DAMAGE) <= neighbor.value(P.DAMAGE))) {
		
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
	    if (ValidateAction.breach(s, neighbor) &&
		(count <= lowestCount) &&
		(s.target().value(P.DAMAGE) <= neighbor.value(P.DAMAGE))) {
		
		lowestCount = count;
		s.heading = d;
	    }
	}
    }
    
    public static void capture(Site s) {
	int lowestCount = Integer.MAX_VALUE;
	for (Direction d : Site.CARDINALS) {
	    Site neighbor = s.neighbors.get(d);
	    int count = 0;
	    for (Site n : neighbor.neighbors.values())
		if (n.get(State.MINE))
		    count++;
		else if (n.get(State.ENEMY))
		    count--;
	    if (ValidateAction.capture(s, neighbor) &&
		(count <= lowestCount) &&
		((s.target().value(P.DAMAGE) <= neighbor.value(P.DAMAGE)) || (s.units <= Stats.maxGenerator))) {

		lowestCount = count;
		s.heading = d;
	    }
	}
    }

    public static void evade(Site s) {
	
    }

    public static void lock(Site s, float mapScaling) {
	if (s.moving() && (s.units > Stats.maxGenerator) && (mapScaling > Parameters.evadeThreshold)) {
	    float unitBuildUp = 0;
	    for (Site n : s.target().neighbors.values())
	    	if (n.get(State.ENEMY) && (n.units > unitBuildUp))
	    	    unitBuildUp = n.units;
	    if (unitBuildUp < s.units * 1.2f)
		for (Site n : s.target().neighbors.values()) {
		    n.set(P.LOCKED, 0);
		    // boolean enemyAdj = s.get(State.ENEMY);
		    // if (!enemyAdj)
		    //     for (Site en : n.neighbors.values()) {
		    // 	    enemyAdj = en.get(State.ENEMY);
		    // 	if (enemyAdj)
		    // 	    break;
		    //     }
		    // if (enemyAdj)
		    //     for (Site en : n.neighbors.values())
		    // 	    en.set(State.LOCKED);
		}
	}
    }
}
