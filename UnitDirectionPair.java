public class UnitDirectionPair implements Comparable<UnitDirectionPair> {
    public final Site site;
    public final Direction direction; 
	
    public UnitDirectionPair(Site site, Direction direction) {
	this.site = site;
	this.direction = direction;
    }

    @Override
    public boolean equals(Object obj) {
	if (obj instanceof UnitDirectionPair)
	    return ((UnitDirectionPair)obj).equals(this);
	return false;
    }

    @Override
    public int compareTo(UnitDirectionPair o) {
	return this.site.compareTo(o.site);
    }

    public String toString() {
	return site.toString() + ", " + direction.ordinal();
    }
}
