package game;

import java.util.HashMap;
import java.util.Map.Entry;

public class Stats {

    //static
    public static float totalGenerator = 0;
    public static float minGenerator = Float.MAX_VALUE;
    public static float maxGenerator = -Float.MAX_VALUE;

    public static float defenseRange;
    public static float enemyDistance;
    public static float frontierRange;   

    //dynamic
    public static float minExplore = Float.MAX_VALUE;
    public static float maxExplore = -Float.MAX_VALUE;

    public static float minDamage = Float.MAX_VALUE;
    public static float maxDamage = -Float.MAX_VALUE;

    public static float minStrategy = Float.MAX_VALUE;
    public static float maxStrategy = -Float.MAX_VALUE;

    public static float minDefense = Float.MAX_VALUE;
    public static float maxDefense = -Float.MAX_VALUE;

    public static float minStrength = Float.MAX_VALUE;
    public static float maxStrength = -Float.MAX_VALUE;

    public static float totalUnexploredGenerator = 0;

    public static HashMap<Byte, Integer> siteCounter = new HashMap<Byte, Integer>();
    
    public static String siteCounts() {
	StringBuilder sb = new StringBuilder();
	for (Entry<Byte, Integer> count : siteCounter.entrySet()) {
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

	minStrategy = Float.MAX_VALUE;
	maxStrategy = -Float.MAX_VALUE;

	minDefense = Float.MAX_VALUE;
	maxDefense = -Float.MAX_VALUE;

	minStrength = Float.MAX_VALUE;
	maxStrength = -Float.MAX_VALUE;
    }
}
