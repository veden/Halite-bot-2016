import java.util.ArrayList;
import java.util.HashMap;

import gnu.trove.iterator.TShortIterator;
import gnu.trove.list.array.TShortArrayList;
import gnu.trove.map.hash.TShortFloatHashMap;

public class LayerProcess implements ProcessSite {
    
    private int maxRadius;
    public HashMap<Site, Ring> siteToRings;
    private GrabLayerProcess glp;
    
    public LayerProcess(int maxRadius) {
	this.maxRadius = maxRadius;
	siteToRings = new HashMap<Site, Ring>();
	glp = new GrabLayerProcess();
    }

    @Override
    public void process(Site center) {

	Ring ring = buildRing(center, maxRadius);
	siteToRings.put(center, ring);
	
	if (center.generator > 0) {	    	    
	    float generator = center.getValue();
	    
	    TShortArrayList[] rings = ring.layers;
	    TShortFloatHashMap ringTotals = ring.totals;

	    float totalUnits;
	    float reduction;
	    float generatorReduction;
	    float damageScore = MyBot.tables.damageSet[center.units];
	    float damageReduction = 0f;

	    float total;
	    TShortArrayList ringSites;

	    Site ringSite;

	    byte owner = MyBot.ai.getOwner(center.x, center.y);
	    boolean enemy = false;
	    boolean neutral = false;
	    if (owner == 0)
		neutral = true;
	    else if (owner != MyBot.ID)
		enemy = true;
	    
	    for (int r = 0; r <= maxRadius; r++) {
	    	ringSites = rings[r];
		if (ringSites.size() > 0) {
		    totalUnits = MyBot.tables.totalUnitSet[r];
		    reduction = MyBot.tables.reductionSet[r];
		    generatorReduction = generator * reduction;
		    if (enemy && (r <= 2))
			damageReduction = damageScore * reduction;
		    for (int x = 0; x < ringSites.size(); x++) {
			short i = ringSites.getQuick(x);
			ringSite = MyBot.map.getSite(i);
			total = ringTotals.get(i);
			if (neutral && (ringSite.units != 0)) {
			    ringSite.explore += generatorReduction * (float)Math.pow((1 - (total/totalUnits)), 0.8f);
			} else if (enemy) {
			    ringSite.defense += generatorReduction * (float)Math.pow((1 - (total/totalUnits)), 0.8f);
			    if (r <= 2)
				ringSite.damage += damageReduction;
			}
		    }
		}
	    }
	}
    }
    
    private Float findPreviousUnits(Site ringSite, TShortFloatHashMap ringTotals) {
	Float lowestUnits = null;
	int width = MyBot.map.width;
	int height = MyBot.map.height;
        short i = (short)(Utils.safeCoordinate(ringSite.x - 1, width) * height + ringSite.y);
	float total = ringTotals.get(i);
	if ((total != -Float.MAX_VALUE) && ((lowestUnits == null) || (lowestUnits > total)))
	    lowestUnits = total;
        i = (short)(Utils.safeCoordinate(ringSite.x + 1, width) * height + ringSite.y);
	total = ringTotals.get(i);
	if ((total != -Float.MAX_VALUE) && ((lowestUnits == null) || (lowestUnits > total)))
	    lowestUnits = total;		    
	i = (short)(ringSite.x * height + Utils.safeCoordinate(ringSite.y - 1, height));
	total = ringTotals.get(i);
	if ((total != -Float.MAX_VALUE) && ((lowestUnits == null) || (lowestUnits > total)))
	    lowestUnits = total;
	i = (short)(ringSite.x * height + Utils.safeCoordinate(ringSite.y + 1, height));
	total = ringTotals.get(i);
	if ((total != -Float.MAX_VALUE) && ((lowestUnits == null) || (lowestUnits > total)))
	    lowestUnits = total;
	return lowestUnits;
    }

    private Ring buildRing(Site center, int maxRadius) {
	ArrayList<TShortArrayList> rings = new ArrayList<TShortArrayList>();
	for (int i = 0; i <= maxRadius; i++)
	    rings.add(new TShortArrayList());

	TShortFloatHashMap ringTotals = new TShortFloatHashMap();

	glp.set(center, rings, ringTotals);
	
	MyBot.map.processRegion(center.x-maxRadius,
				center.y-maxRadius,
				center.x+maxRadius,
				center.y+maxRadius,
				glp);
	    
	ringTotals.put(Utils.coordinateToIndex(center.x, center.y), (float)center.units);

	for (int r = 1; r <= maxRadius; r++) {
	    for (TShortIterator cursor = rings.get(r).iterator(); cursor.hasNext();) {
		short i = cursor.next();
		Site ringSite = MyBot.map.getSite(i);
		Float lowestUnits = findPreviousUnits(ringSite, ringTotals);
		if (lowestUnits != null) {
		    if (MyBot.ai.isNeutral(ringSite.x, ringSite.y))
			ringTotals.put(i, lowestUnits+ringSite.units);
		    else
			ringTotals.put(i, lowestUnits+ringSite.initialUnits);
		} else if (r+1 <= maxRadius) {
		    cursor.remove();
		    rings.get(r+1).add(i);
		} else
		    cursor.remove();
	    }
	}
	
	return new Ring(rings.toArray(new TShortArrayList[rings.size()]), ringTotals);
    }

    private class GrabLayerProcess implements ProcessSite {
	private ArrayList<TShortArrayList> rings;
	private TShortFloatHashMap ringTotals;
	private Site center;
	
	public void set(Site center, ArrayList<TShortArrayList> rings, TShortFloatHashMap ringTotals) {
	    this.center = center;
	    this.rings = rings;
	    this.ringTotals = ringTotals;
	}
	
	@Override
	public void process(Site s) {
	    if (MyBot.ai.getOwner(s.x, s.y) == MyBot.ai.getOwner(center.x, center.y)) {
		float distance = MyBot.map.manhattanDistance(center, s);
		if (distance <= maxRadius) {
		    short i = Utils.coordinateToIndex(s.x, s.y);
		    rings.get((int)distance).add(i);
		    ringTotals.put(i, -Float.MAX_VALUE);
		}
	    }
	}
    }
}
