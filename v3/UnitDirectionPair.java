public class UnitDirectionPair implements Comparable<UnitDirectionPair> {
    public final Tile tile;
    public final Direction direction; 
	
    public UnitDirectionPair(Tile tile, Direction direction) {
	this.tile = tile;
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
	return this.tile.compareTo(o.tile);
    }

    public String toString() {
	return tile.toString() + ", " + direction.ordinal();
    }
}
