package game;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;

public class Site implements Comparable<Site> {    

    public static enum State {
	USED, BATTLE, INTERIOR, FRONTIER, BORDER, UNEXPLORED,
        FIELD, MINE, NEUTRAL, ENEMY, EXPLORE_CANDIDATE, READY
    }

    public static HashMap<Integer, Integer> siteGeneratorCounts = new HashMap<Integer, Integer>();
    
    public static final short MAX_CAPTURE_STRENGTH = 20;
    public static final short MAX_STRENGTH = 255;
    public static final short MAX_STRENGTH_LOSSY = 270;

    public short id;
    public short units; // originally strength
    public byte generator; // originally production
    public final byte x;
    public final byte y;

    public byte owner;
    public EnumSet<State> status = EnumSet.noneOf(State.class);
    
    public short incoming;
    public short outgoing;

    public Direction heading;

    public EnumMap<Direction, Site> neighbors = new EnumMap<Direction, Site>(Direction.class);

    public float accumulatorThreshold = 4f;
    public float explore;
    public float strength;
    public float defense;
    public float damage;
    private float exploreValue = -Float.MAX_VALUE;
 
    public Site(byte x, byte y) {
	this.x = x;
	this.y = y;
	this.id = (short)(x * Harness.map.height + y);
    }

    public void setGenerator(byte generator) {
	this.generator = generator;
	if (!siteGeneratorCounts.containsKey(generator))
	    siteGeneratorCounts.put((int)generator, 0);
	siteGeneratorCounts.put((int)generator, siteGeneratorCounts.get((int)generator)+1);
    }

    public float getExploreValue() {
	if (generator != 0) {
	    if (exploreValue == -Float.MAX_VALUE) {
		//		int count = siteGeneratorCounts.get((int)generator); 
		// float v = 0;// (count / Harness.map.totalSites);
		// v += (count * generator) / (float)Harness.map.totalGenerator;
		exploreValue = (1f - (units / (float)MAX_STRENGTH)) * (generator / (float)Harness.map.maxGenerator) * (1f - ((units / (float)generator) / (float)MAX_STRENGTH));
	    }
	} else
	    return 0;
	return exploreValue;
    }
 
    public boolean aboveActionThreshold() {
	return generator * accumulatorThreshold < units;
    }

    public boolean aboveCombatThreshold() {
	return generator * accumulatorThreshold * 0.75 < units;
    }

    public String encodeMove() {
	return x + " " + y + " " + Direction.encodeDirection(heading) + " ";
    }

    public Site target() {
	switch(heading) {
	case STILL:
	    return this;
	default:
	    return neighbors.get(heading);
	}
    }

    public void set(Site.State s) {
	status.add(s);
    }

    public void remove(Site.State s) {
	status.remove(s);
    }
    
    public boolean get(Site.State s) {
	return status.contains(s);
    }
 
    public void reset() {
	status.clear();
	explore = 0;
	strength = 0;
	defense = 0;
	damage = 0;
	incoming = 0;
	outgoing = 0;
	heading = Direction.STILL;
    }

    public String toString() {
	return "x-" + x + " y-" + y + " gStr-" + units + " gProd-" + generator + " prod-" + explore + " str-" + strength + " def-" + defense;
    }

    public String encodeString() {
	return x + " " + y + " " + units + " " + generator + " " + owner + " " + explore + " " + strength + " " + defense + " " + damage + " " + get(Site.State.BATTLE);
    }
     
    @Override
    public int compareTo(Site t) {
	float v = (t.explore - this.explore);
	if (v == 0) {
	    v = (t.strength - this.strength);
	    if (v == 0)
		return this.id - t.id;
	}
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
}
