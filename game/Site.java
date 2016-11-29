package game;


public class Site implements Comparable<Site> {    
    public static final short MAX_STRENGTH = 255;
    public static final short MAX_STRENGTH_LOSSY = 270;

    public short id;
    public short units; // originally strength
    public short initialUnits;
    public byte generator; // originally production
    public final byte x;
    public final byte y;

    public byte owner;
    public byte status;
    
    public short incoming;
    public short outgoing;

    public Direction heading;

    public Site[] neighbors = new Site[4]; // N, E, S, W

    public byte[] distances;
    
    public float accumulatorThreshold = 5f;
    public float explore;
    public float strength;
    public float defense;
    public float damage;
    public float value = -Float.MAX_VALUE;
 
    public Site(byte x, byte y) {
	this.x = x;
	this.y = y;
	this.id = (short)(x * Harness.map.height + y);
    }

    public float getValue() {
	if (value == -Float.MAX_VALUE)
	    value = (1f / (initialUnits / (1f * generator))) * (0.1f * generator) * (1f - (initialUnits / MAX_STRENGTH));
	return value;
    }
 
    public boolean aboveActionThreshold() {
	return generator * accumulatorThreshold < units;
    }

    public boolean aboveCombatThreshold() {
	return generator * accumulatorThreshold * 0.75 < units;
    }

    public byte distanceTo(Site s) {
	return distances[s.id];
    }

    public String encodeMove() {
	return x + " " + y + " " + Direction.encodeDirection(heading) + " ";
    }
    
    public boolean mine() {
	return owner == Harness.map.bot.id;
    }

    public boolean neutral() {
	return owner == 0;
    }

    public boolean enemy() {
	return !mine() && !neutral();
    }

    public void setObjective() {
	status = (byte)(status | 0x01);
    }
 
    public boolean objective() {
	return (byte)(status & 0x01) == 1;
    }
    public boolean used() {
	return (byte)(status & 0x02) == 2;
    }
    public void setUsed() {
	status = (byte)(status | 2);
    }
    
    public void setBattle() {
	status = (byte)(status | 4); 
    }
 
    public boolean battle() {
	return (byte)(status & 0x04) == 4;
    }

    public void setFront() {
	status = (byte)(status | 8);
    }
   
    public boolean front() {
	return (byte)(status & 0x08) == 8;
    }
 
    public void reset() {	
	if (owner == Harness.map.bot.id)
	    status = 0;
	else
	    status = (byte)(status & 0xFE);
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
	return x + " " + y + " " + units + " " + generator + " " + owner + " " + explore + " " + strength + " " + defense + " " + damage + " " + objective();
    }
     
    @Override
    public int compareTo(Site t) {
	float v = (t.explore - this.explore);
	if (v == 0) {
	    v = (t.strength - this.strength);
	    if (v == 0)
		return (this.x - t.x) - (this.y - t.y);
	}
	return v>0?1:-1;
    }
    @Override
    public boolean equals(Object obj) {
	if (obj instanceof Site) {
	    Site that = (Site)obj;
	    return ((that.x == x) && (that.y == y));
	}
	return false;
    }
}
