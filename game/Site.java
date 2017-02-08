package game;

import java.util.EnumMap;
import java.util.EnumSet;

import logic.Constants;
import logic.Constants.A;
import logic.Constants.D;
import logic.Constants.F;
import logic.Constants.P;
import logic.Constants.S;
import logic.Parameters;

public class Site {
    public int id;
    public int units;
    public final int x;
    public final int y;

    public int owner;
    public EnumSet<S> status = EnumSet.noneOf(S.class);
   
    public int incoming;
    public int outgoing;

    public D heading = D.STILL;
    public A action = A.IDLE;

    public EnumMap<D, Site> neighbors = new EnumMap<D, Site>(D.class);
    public EnumMap<P, Float> properties = new EnumMap<P, Float>(P.class);
    public EnumMap<F, Float> fields = new EnumMap<F, Float>(F.class);

    public float stagingValue = 0f;
    
    public float sitePotential = 0f;
 
    public Site(int x, int y, int height) {
	this.x = x;
	this.y = y;
	this.id = (int)(x * height + y);
	for (P p : P.values())
	    set(p, 0f);
	for (F f : F.values())
	    set(f, 0f);
	set(P.EXPLORE_VALUE, -Float.MAX_VALUE);
	set(P.ACCUMULATOR, Parameters.baseAccumulator);
    }

    public float generateExploreValue() {
	float v = 0;
	if (v(P.GENERATOR) != 0)
	    v = ((1f - (units/Constants.MAX_UNITS)) *
	    	 ((Parameters.generatorWeight * ((v(P.GENERATOR) / Stats.maxGenerator))) +
		  (Parameters.siteCostWeight * ((1f / ((float)units / v(P.GENERATOR))) / Stats.maxGenerator)) +
		  (Parameters.sitePotentialWeight * (sitePotential / Stats.maxSitePotential)) +
		  (Parameters.siteCountWeight * (1f - ((Stats.siteCounter.get(v(P.GENERATOR)) / Stats.totalSites)))) +
		  (Parameters.generatorTotalWeight * ((Stats.siteCounter.get(v(P.GENERATOR)) * v(P.GENERATOR)) / Stats.totalGenerator))
		  ));
	set(P.EXPLORE_VALUE, v);
	return v;
    }


    public void assign(int o, int prevO, int myId, boolean objective, float mapScaling) {
	reset();
	if (o == prevO)
	    age();
	else {
	    set(P.AGE, 0);
	    set(P.ACCUMULATOR, Math.max(Parameters.baseAccumulator, v(P.ACCUMULATOR) - 0.5f));
	}
	owner = o;
	if (owner == 0) {
	    set(S.NEUTRAL);
	    if (objective)
		set(S.OBJECTIVE);
	} else if (owner == myId)
	    set(S.MINE);
	else
	    set(S.ENEMY);
    }
    
    public boolean aboveActionThreshold() {
	return v(P.GENERATOR) * v(P.ACCUMULATOR) < units;
    }

    public boolean aboveCombatThreshold() {
	return v(P.GENERATOR) * (v(P.ACCUMULATOR) * 0.65) < units;
    }

    public String encodeMove() {
	return x + " " + y + " " + encodeDirection(heading) + " ";
    }

    public boolean moving() {
	return heading != D.STILL;
    }

    public Site target() {
	switch(heading) {
	case STILL:
	    return this;
	default:
	    return neighbors.get(heading);
	}
    }
    
    public void commit(F field) {
	set(field, stagingValue);
	stagingValue = 0;
    }

    public void accumulate(P property, float v) {
	if (!properties.containsKey(property))
	    properties.put(property, 0f);
	properties.put(property, properties.get(property) + v);
    }
    
    public void set(S s) {
	status.add(s);
    }

    public void set(P property, float v) {
	properties.put(property, v);
    }

    public void set(F field, float v) {
        fields.put(field, v);
    }

    public void remove(S s) {
	status.remove(s);
    }
    
    public boolean is(S s) {
	return status.contains(s);
    }

    public boolean isNot(S s) {
	return !status.contains(s);
    }

    public float v(P property) {
	return properties.get(property);
    }

    public float v(F field) {
	return fields.get(field);
    }
 
    public void reset() {
	status.clear();
	stagingValue = 0;

	for (F field : F.values())
	    set(field, 0);

	set(P.DISTANCE, 0);
	set(P.ENEMY_UNITS, 0);
	set(P.ALLOWED_UNITS, Constants.MAX_UNITS);
	action = A.IDLE;
	incoming = 0;
	outgoing = 0;
	heading = D.STILL;
    }

    public void age() {
	float currentAge = v(P.AGE)+1;
	if (currentAge == 15) {
	    float v = Math.min(v(P.ACCUMULATOR)+1f, 90.0f / v(P.GENERATOR));
	    set(P.ACCUMULATOR, v);
	    set(P.AGE, 0);
	} else
	    set(P.AGE, currentAge);
    }

    public int compressAttributes() {
	int result = 0;
	int highest = 0;
	for (Enum<S> p : S.values()) {
	    if (p.ordinal() == S.MINE.ordinal())
		break;
	    result = result | (status.contains(p) ? 1 : 0) << p.ordinal();
	    if (p.ordinal() > highest)
		highest = p.ordinal();
	}
	return result | 1 << (S.MINE.ordinal());
    }

    public String encodeAttributes() {
	return units + " " + owner + " " + v(F.EXPLORE) + " " + v(F.REINFORCE) + " " + v(F.DAMAGE) + " " + v(P.ALLOWED_UNITS) + " " + v(P.AGE) + " " + v(P.ACCUMULATOR) + " " + v(P.EXPLORE_VALUE) + " " + v(P.DISTANCE) + " " + action.ordinal() + " " + v(P.ENEMY_UNITS) + " " + compressAttributes();
    }

    public String encodeSite() {
	return x + " " + y + " " + v(P.GENERATOR);
    }
     
    @Override
    public boolean equals(Object obj) {
	if (obj instanceof Site) {
	    Site that = (Site)obj;
	    return (this.id - that.id) == 0;
	}
	return false;
    }

    public static int encodeDirection(D d) {
	int o;
	if (d == D.NORTH)
	    o = 1;
	else if (d == D.EAST)
	    o = 2;
	else if (d == D.SOUTH)
	    o = 3;
	else if (d == D.WEST)
	    o = 4;
	else
	    o = 0;
	return o;
    }

    public static D reverse(D d) {
	if (d == D.NORTH)
	    return D.SOUTH;
	else if (d == D.EAST)
	    return D.WEST;
	else if (d == D.SOUTH)
	    return D.NORTH;
	else if (d == D.WEST)
	    return D.EAST;
	return D.STILL;
    }
}
