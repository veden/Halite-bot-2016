package game;

import java.util.EnumMap;
import java.util.EnumSet;

import logic.Parameters;

public class Site implements Comparable<Site> {    

    public static enum State {
	BATTLE, FRONTIER, UNEXPLORED, INTERIOR, BORDER, OPEN,
	READY, OBJECTIVE, COMBAT_READY, GATE, LOCKED, 
	
	USED, MINE, NEUTRAL, ENEMY
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
    public int generator; // originally production
    public final int x;
    public final int y;

    public int owner;
    public EnumSet<State> status = EnumSet.noneOf(State.class);
   
    public int incoming;
    public int outgoing;

    public Direction heading = Direction.STILL;

    public EnumMap<Direction, Site> neighbors = new EnumMap<Direction, Site>(Direction.class);

    public float stagingValue = 0f;
    
    public float accumulatorThreshold = 5f;
    public float sitePotential = 0f;
    public float explore;
    public float reinforce;
    public float damage; 
    private float exploreValue = -Float.MAX_VALUE;
 
    public Site(int x, int y, int height) {
	this.x = x;
	this.y = y;
	this.id = (int)(x * height + y);
    }

    public float getExploreValue() {
	if (generator != 0) {
	    if (exploreValue == -Float.MAX_VALUE)
		exploreValue = ((1f - (units/Site.MAX_STRENGTH)) *
				((Parameters.generatorWeight * ((generator / Stats.maxGenerator))) +
				 (Parameters.siteCostWeight * ((1f / ((float)units / generator)) / Stats.maxGenerator)) +
				 (Parameters.sitePotentialWeight * (sitePotential / Stats.maxSitePotential)) +
				 (Parameters.siteCountWeight * (1f - (Stats.siteCounter.get(generator) / Stats.totalSites))) +
				 (Parameters.generatorTotalWeight * ((Stats.siteCounter.get(generator) * generator) / Stats.totalGenerator))
				 ));
	} else
	    return 0;
	return exploreValue;
    }
 
    public boolean aboveActionThreshold() {
	return generator * accumulatorThreshold < units;
    }

    public boolean aboveCombatThreshold() {
	return generator * (accumulatorThreshold * 0.5) < units;
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

    public void set(State s) {
	status.add(s);
    }

    public void remove(State s) {
	status.remove(s);
    }
    
    public boolean get(State s) {
	return status.contains(s);
    }
 
    public void reset() {
	status.clear();
	stagingValue = 0;
	explore = 0;
	reinforce = 0;
	damage = 0;
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
    
    public String encodeAttributes() {
	return units + " " + owner + " " + explore + " " + reinforce + " " + damage + " " + compressAttributes();
    }

    public String encodeSite() {
	return x + " " + y + " " + generator;
    }
     
    @Override
    public int compareTo(Site t) {
	float v = (t.explore - this.explore);
	if (v == 0)
	    return this.id - t.id;
	return v>0?1:-1;
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
