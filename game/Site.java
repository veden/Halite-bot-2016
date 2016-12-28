package game;

import java.util.EnumMap;
import java.util.EnumSet;

public class Site implements Comparable<Site> {    

    public static enum State {
	USED, BATTLE, INTERIOR, FRONTIER, BORDER, UNEXPLORED,
        OPEN, MINE, NEUTRAL, ENEMY, OBJECTIVE, READY, SPEAR,
	COMBAT_READY, GATE, EXPLORE_CANDIDATE, LOCKED
    }

    public static enum Direction {
	NORTH, EAST, SOUTH, WEST, STILL;
    }
    
    public static final EnumSet<Direction> DIRECTIONS = EnumSet.of(Direction.STILL, Direction.NORTH, Direction.EAST, Direction.SOUTH, Direction.WEST);
    public static final EnumSet<Direction> CARDINALS = EnumSet.of(Direction.NORTH, Direction.EAST, Direction.SOUTH, Direction.WEST);

    public static final float MAX_STRENGTH = 255f;
    public static float MAX_STRENGTH_LOSSY = 0f; //set during network init

    public short id;
    public short units; // originally 
    public byte generator; // originally production
    public final byte x;
    public final byte y;

    public byte owner;
    public EnumSet<State> status = EnumSet.noneOf(State.class);
   
    public short incoming;
    public short outgoing;

    public Direction heading = Direction.STILL;

    public EnumMap<Direction, Site> neighbors = new EnumMap<Direction, Site>(Direction.class);

    public float stagingValue = 0;
    
    public float accumulatorThreshold = 5f;
    public float sitePotential = 0f;
    public float explore;
    public float reinforce;
    public float damage; 
    private float exploreValue = -Float.MAX_VALUE;
 
    public Site(byte x, byte y, int height) {
	this.x = x;
	this.y = y;
	this.id = (short)(x * height + y);
    }

    public float getExploreValue() {
	if (generator != 0) {
	    if (exploreValue == -Float.MAX_VALUE)
		exploreValue = ((1f - (units/Site.MAX_STRENGTH)) *
				((0.35f * ((generator / Stats.maxGenerator))) +
				 (0.55f * ((1f / ((float)units / generator)) / Stats.maxGenerator)) +
				 (0.20f * (sitePotential / Stats.maxSitePotential)) +
				 (0.25f * (1 - (Stats.siteCounter.get(generator) / Stats.totalSites))) +
				 (0.25f * ((Stats.siteCounter.get(generator) * generator) / Stats.totalGenerator))
				 ));
	} else
	    return 0;
	return exploreValue;
    }
 
    public boolean aboveActionThreshold() {
	return generator * accumulatorThreshold < units;
    }

    public boolean aboveCombatThreshold() {
	return generator * (accumulatorThreshold * 0.75) < units;
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
	// boolean objective = get(State.OBJECTIVE);
	// boolean neutral = get(State.NEUTRAL);
	status.clear();
	// if (neutral && objective)
	//     set(State.OBJECTIVE);
	stagingValue = 0;
	explore = 0;
	reinforce = 0;
	damage = 0;
	incoming = 0;
	outgoing = 0;
	heading = Direction.STILL;
    }

    public String toString() {
	return "x-" + x + " y-" + y + " gStr-" + units + " gProd-" + generator + " prod-" + explore + " def-" + reinforce;
    }

    public String encodeString() {
	return x + " " + y + " " + units + " " + generator + " " + owner + " " + explore + " " + reinforce + " " + damage + " " + get(State.BATTLE) + " " + get(State.FRONTIER) + " " + get(State.UNEXPLORED) + " " + get(State.INTERIOR) + " " + get(State.BORDER) + " " + get(State.OPEN) + " " + get(State.READY) + " " + get(State.SPEAR) + " " + get(State.OBJECTIVE) + " " + get(State.COMBAT_READY) + " " + get(State.GATE) + " " + get(State.EXPLORE_CANDIDATE) + " " + get(State.LOCKED);
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
