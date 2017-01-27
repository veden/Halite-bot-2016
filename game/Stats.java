package game;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map.Entry;

public class Stats {

    //static
    public static float totalGenerator = 0;
    public static float totalSites = 0;
    public static float minGenerator = Float.MAX_VALUE;
    public static float maxGenerator = -Float.MAX_VALUE;

    public static float minExploreValue = Float.MAX_VALUE;
    public static float maxExploreValue = -Float.MAX_VALUE;
    
    //dynamic
    public static float minExplore = Float.MAX_VALUE;
    public static float maxExplore = -Float.MAX_VALUE;

    public static float minDamage = Float.MAX_VALUE;
    public static float maxDamage = -Float.MAX_VALUE;

    public static float minReinforce = Float.MAX_VALUE;
    public static float maxReinforce = -Float.MAX_VALUE;

    public static float totalUnexploredGenerator = 0;

    public static HashMap<Float, Float> siteCounter = new HashMap<Float, Float>();
    public static float maxSitePotential;
    
    public static String siteCounts() {
	StringBuilder sb = new StringBuilder();
	ArrayList<Entry<Float, Float>> counts = new ArrayList<Entry<Float, Float>>();
	counts.addAll(siteCounter.entrySet());
	Collections.sort(counts, new Comparator<Entry<Float, Float>>() {
		public int compare(Entry<Float, Float> e1, Entry<Float, Float> e2) {
		    return (e1.getKey() - e2.getKey()) > 0 ? 1 : -1;
		}
	    });
	for (Entry<Float, Float> count : counts) {
	    if (sb.length() > 0)
		sb.append(" ");
	    sb.append(count.getKey() + " " + count.getValue());
	}
	return sb.toString();
    }
    
    public static void reset() {
	totalUnexploredGenerator = 0;
	
	minExplore = Float.MAX_VALUE;
	maxExplore = -Float.MAX_VALUE;

	minDamage = Float.MAX_VALUE;
	maxDamage = -Float.MAX_VALUE;

	minReinforce = Float.MAX_VALUE;
	maxReinforce = -Float.MAX_VALUE;
    }
}
