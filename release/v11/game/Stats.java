package game;

import java.util.HashMap;
import java.util.Map.Entry;

public class Stats {

    //static
    public static float totalGenerator = 0;
    public static float totalSites = 0;
    public static float minGenerator = Float.MAX_VALUE;
    public static float maxGenerator = -Float.MAX_VALUE;

    //dynamic
    public static float minExplore = Float.MAX_VALUE;
    public static float maxExplore = -Float.MAX_VALUE;

    public static float minDamage = Float.MAX_VALUE;
    public static float maxDamage = -Float.MAX_VALUE;

    public static float minReinforce = Float.MAX_VALUE;
    public static float maxReinforce = -Float.MAX_VALUE;

    public static float totalUnexploredGenerator = 0;

    public static HashMap<Integer, Integer> siteCounter = new HashMap<Integer, Integer>();
    public static float maxSitePotential;
    
    public static String siteCounts() {
	StringBuilder sb = new StringBuilder();
	for (Entry<Integer, Integer> count : siteCounter.entrySet()) {
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
