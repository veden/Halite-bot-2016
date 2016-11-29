
public class Site implements Comparable<Site> {

    public short units; // originally strength
    public short initialUnits;
    public byte generator; // originally production
    public final byte x;
    public final byte y;

    public byte owner;
    public byte status;

    public short incoming;
    public short outgoing;
    
    public float explore;
    public float strength;
    public float defense;
    public float damage;
    public float value = -Float.MAX_VALUE;
    
    public Site(byte x, byte y) {
	this.x = x;
	this.y = y;
    }

    public float getValue() {
	if (value == -Float.MAX_VALUE)
	    value = (1 / (initialUnits / (1f * generator))) * (0.1f * generator) * (1 - (initialUnits/Constants.SITE_MAX_STRENGTH));
	return value;
    }
    
    public boolean aboveActionThreshold() {
	return generator * Constants.SITE_ACCUMULATION_MULTIPLIER < units;
    }

    public boolean aboveCombatThreshold() {
	return generator * Constants.SITE_ACCUMULATION_MULTIPLIER * 0.75 < units;
    }

    public boolean mine() {
	return owner == MyBot.ID;
    }

    public boolean neutral() {
	return owner == 0;
    }

    public boolean enemy() {
	return !mine() && !neutral();
    }

    public void setObjective() {
	status = status | 0x01;
    }
    
    public boolean objective() {
	return status & 0x01 == 1;
    }

    public boolean used() {
	return status & 0x02 == 2;
    }

    public void setBattle() {
	status = status | 4; 
    }
    
    public boolean battle() {
	return status & 0x04 == 4;
    }

    public boolean front() {
	return status & 0x08 == 8;
    }
    
    public void reset() {	
	if (owner == MyBot.ID)
	    status = 0;
	else
	    status = status & 0xFE;
	explore = 0;
	strength = 0;
	defense = 0;
	damage = 0;
	incoming = 0;
	outgoing = 0;
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
