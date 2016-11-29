public class Tile implements Comparable<Tile> {

    private static int counter = 1;
    private static float maxGenerator = -Float.MAX_VALUE;
    private static float minGenerator = Float.MAX_VALUE;
    private static float maxUnit = -Float.MAX_VALUE;
    private static float minUnit = Float.MAX_VALUE;
    
    private static float scaleGenerator = -1.0f;
    private static float averageGenerator = -1.0f;
    private static float averageUnit = -1.0f;
    private static boolean locked = false;

    private Site site;
    public float production;
    public float strength;
    public float defense;
    public int x;
    public int y;
    public int openings = 0;
    public int ambushes = 0;
    public boolean used;
    public final int id;
    
    public Tile(int x, int y, Site site) {
	this.id = counter++;
	this.site = site;
	this.x = x;
	this.y = y;
	used = false;
	if (site.production > maxGenerator)
	    maxGenerator = site.production;
	if (site.production < minGenerator)
	    minGenerator = site.production;
	if (site.strength < minUnit)
	    minUnit = site.strength;
	if (site.strength > maxUnit)
	    maxUnit = site.strength;
	averageUnit += site.strength;
	averageGenerator += site.production;
    }

    public static float getGeneratorScaling() {
	if (scaleGenerator == -1)
	    scaleGenerator = (Constants.SITE_MAX_STRENGTH / maxGenerator);
	return scaleGenerator;
    }

    public static float getGeneratorMin() {
	return minGenerator;
    }

    public static float getGeneratorMax() {
	return maxGenerator;
    }

    public static float getUnitMin() {
	return minUnit;
    }

    public static float getUnitMax() {
	return maxUnit;
    }

    public static float getGeneratorAverage() {
	if (!locked) {
	    averageGenerator /= counter;
	    averageUnit /= counter;
	    locked = true;
	}
	return averageGenerator;
    }

    public static float getUnitAverage() {
	if (!locked) {
	    averageGenerator /= counter;
	    averageUnit /= counter;
	    locked = true;
	}
	return averageUnit;
    }

    private int getOwner() {
	return site.owner;
    }

    public boolean mine() {
	return MyBot.ID == getOwner();
    }

    public float score() {
	if (production == 0)
	    return strength;
	return production;
    }
    
    public boolean neutral() {
	return getOwner() == 0;
    }

    public boolean enemy() {
	return !mine() && !neutral();
    }
    
    public boolean aboveUnitThreshold() {
	return getGenerator() * Constants.SITE_ACCUMULATION_MULTIPLIER < getUnits();
    }
    
    public int getGenerator() {
	return site.production;
    }

    public int getUnits() {
	return site.strength;
    }

    public void updateTile(Site site) {
	this.site = site;
	used = false;
	production = 0;
	strength = 0;
	defense = 0;
    } 
    
    public String toString() {
	return "x-" + x + " y-" + y + " gStr-" + site.strength + " gProd-" + site.production + " o-" + site.owner + " prod-" + production + " str-" + strength + " def-" + defense + " open-" + openings + " amb-" + ambushes;
    }
    
    @Override
    public int compareTo(Tile t) {
	float v = (t.score() - this.score());
	if (v == 0)
	    return this.id - t.id;
	return v>0?1:-1;
    }

    @Override
    public boolean equals(Object obj) {
	if (obj instanceof Tile) {
	    Tile that = (Tile)obj;
	    return ((that.x == this.x) && (that.y == this.y));
	}
	return false;
    }
}
