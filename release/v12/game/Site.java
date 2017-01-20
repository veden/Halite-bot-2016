package game;

import java.util.EnumMap;
import java.util.EnumSet;

import logic.Parameters;

public class Site {    

    public static enum State {
	BATTLE, FRONTIER, UNEXPLORED, INTERIOR, BORDER, OPEN,
	READY, OBJECTIVE, COMBAT_READY, GATE, LOCKED, 
	
	USED, MINE, NEUTRAL, ENEMY
    }

    public static enum P {
	EXPLORE_VALUE, EXPLORE, REINFORCE, DAMAGE, GENERATOR
    }

    public static enum Direction {
	NORTH, EAST, SOUTH, WEST, STILL;
    }

    public static final EnumSet<Direction> DIRECTIONS = EnumSet.of(Direction.STILL, Direction.NORTH, Direction.EAST, Direction.SOUTH, Direction.WEST);
    public static final EnumSet<Direction> CARDINALS = EnumSet.of(Direction.NORTH, Direction.EAST, Direction.SOUTH, Direction.WEST);

    public static final float MAX_STRENGTH = 255f;
    public static float MAX_STRENGTH_LOSSY = 0f; //set during network init

    public int id;
    public int units; // originally 
    public final int x;
    public final int y;

    public int owner;
    public EnumSet<State> status = EnumSet.noneOf(State.class);
   
    public int incoming;
    public int outgoing;

    public Direction heading = Direction.STILL;

    public EnumMap<Direction, Site> neighbors = new EnumMap<Direction, Site>(Direction.class);
    public EnumMap<P, Float> properties = new EnumMap<P, Float>(P.class);

    public float stagingValue = 0f;
    
    public float accumulatorThreshold = 4.5f;
    public float sitePotential = 0f;
 
    public Site(int x, int y, int height) {
	this.x = x;
	this.y = y;
	this.id = (int)(x * height + y);
	for (P p : P.values())
	    set(p, 0f);
	set(P.EXPLORE_VALUE, -Float.MAX_VALUE);
    }

    public float generateExploreValue() {
	float v = 0;
	if (value(P.GENERATOR) != 0)
	    v = ((1f - (units/Site.MAX_STRENGTH)) *
	    	 ((Parameters.generatorWeight * ((value(P.GENERATOR) / Stats.maxGenerator))) *
		  (Parameters.siteCostWeight * ((1f / ((float)units / value(P.GENERATOR))) / Stats.maxGenerator)) +
		  (Parameters.sitePotentialWeight * (sitePotential / Stats.maxSitePotential)) +
		  (Parameters.siteCountWeight * (1f - ((Stats.siteCounter.get(value(P.GENERATOR)) / Stats.totalSites)))) +
		  (Parameters.generatorTotalWeight * ((Stats.siteCounter.get(value(P.GENERATOR)) * value(P.GENERATOR)) / Stats.totalGenerator))
		  ));
	set(P.EXPLORE_VALUE, v);
	return v;
    }
     
    public boolean aboveActionThreshold() {
	return value(P.GENERATOR) * accumulatorThreshold < units;
    }

    public boolean aboveCombatThreshold() {
	return value(P.GENERATOR) * (accumulatorThreshold * 0.70) < units;
    }

    public String encodeMove() {
	return x + " " + y + " " + encodeDirection(heading) + " ";
    }

    public boolean moving() {
	return heading != Direction.STILL;
    }

    public Site target() {
	switch(heading) {
	case STILL:
	    return this;
	default:
	    return neighbors.get(heading);
	}
    }

    // public void scaleCommit(P property) {
    // 	set(property, value())
    // 	    }
    
    public void commit(P property) {
	set(property, stagingValue);
	stagingValue = 0;
    }

    public void set(State s) {
	status.add(s);
    }

    public void set(P property, float value) {
	properties.put(property, value);
    }

    public void remove(State s) {
	status.remove(s);
    }
    
    public boolean get(State s) {
	return status.contains(s);
    }

    public float value(P property) {
	return properties.get(property);
    }
 
    public void reset() {
	status.clear();
	stagingValue = 0;
	set(P.EXPLORE, 0);
	set(P.REINFORCE, 0);
	set(P.DAMAGE, 0);
	incoming = 0;
	outgoing = 0;
	heading = Direction.STILL;
    }

    public int compressAttributes() {
	int result = 0;
	int highest = 0;
	for (Enum<State> p : State.values()) {
	    if (p.ordinal() > State.LOCKED.ordinal())
		break;
	    result = result | (status.contains(p) ? 1 : 0) << p.ordinal();
	    if (p.ordinal() > highest)
		highest = p.ordinal();
	}
	return result | 1 << (State.USED.ordinal());
    }

    public String encodeExploreValue() {
	return value(P.EXPLORE_VALUE)+"";
    }
    
    public String encodeAttributes() {
	return units + " " + owner + " " + value(P.EXPLORE) + " " + value(P.REINFORCE) + " " + value(P.DAMAGE) + " " + compressAttributes();
    }

    public String encodeSite() {
	return x + " " + y + " " + value(P.GENERATOR);
    }
     
    @Override
    public boolean equals(Object obj) {
	if (obj instanceof Site) {
	    Site that = (Site)obj;
	    return (this.id - that.id) == 0;
	}
	return false;
    }

    public static int encodeDirection(Direction d) {
	int o;
	if (d == Direction.NORTH)
	    o = 1;
	else if (d == Direction.EAST)
	    o = 2;
	else if (d == Direction.SOUTH)
	    o = 3;
	else if (d == Direction.WEST)
	    o = 4;
	else
	    o = 0;
	return o;
    }

    public static Direction reverse(Direction d) {
	if (d == Direction.NORTH)
	    return Direction.SOUTH;
	else if (d == Direction.EAST)
	    return Direction.WEST;
	else if (d == Direction.SOUTH)
	    return Direction.NORTH;
	else if (d == Direction.WEST)
	    return Direction.EAST;
	return Direction.STILL;
    }
}
