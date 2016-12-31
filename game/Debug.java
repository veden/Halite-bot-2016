package game;

import java.io.FileNotFoundException;
import java.io.PrintWriter;

public class Debug {

    public static boolean enabled = true;
          
    private static PrintWriter pw;
    private static PrintWriter r;

    static {
	try {
	    if (enabled) {
		pw = new PrintWriter("/home/veden/haliteFiles/enabled.txt");
		r = new PrintWriter("/home/veden/haliteFiles/replay.txt");
	    }
	} catch (FileNotFoundException e) {}
    }
    
    public static void println(String s) {
	pw.println(s);
    }

    public static void printReplay(String s) {
	r.println(s);
    }

    public static void flushReplay() {
	r.flush();
    }
    
    public static void flush() {
	pw.flush();
    }

    public static void abort(String message) {
	println(message);
	flush();
	throw new RuntimeException();
    }

    public static void startup(GameMap map) {
	if (enabled) {
	    println("starting AI");
	    long start = System.currentTimeMillis();
	    printReplay(map.width + " " + map.height + " " + Stats.minGenerator + " " + Stats.maxGenerator + "--" + Stats.siteCounts());
	    for (int x = 0; x < map.sites.length; x++)
		printReplay(map.sites[x].encodeSite());
	    println((System.currentTimeMillis() - start) + "-start up");
	    println(((Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / (1024 * 1024)) + "MB-used memory");
	    flush();
	}
    }

    public static long startClock(int turn) {
	if (enabled) {
	    println("Frame-" + turn);
	    return System.currentTimeMillis();
	}
	return 0;
    }

    public static void stopClock(GameMap map, int turn, long start) {
	if (enabled) {
	    map.collectStats();

	    println((System.currentTimeMillis() - start) + "-duration");
	    println(((Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / (1024 * 1024)) + "MB-used memory");

	    flush();
	
	    printReplay("----");
	    printReplay(turn + " " + Stats.minExplore + " " + Stats.maxExplore + " " + Stats.minDamage + " " + Stats.maxDamage + " " + Stats.minReinforce + " " + Stats.maxReinforce + " " + Stats.totalUnexploredGenerator);
	    printReplay("===");
	    printReplay(map.bot.toString());
	    printReplay(map.enemiesToReplay());
	    printReplay("===");
	    for (int x = 0; x < map.sites.length; x++)
		printReplay(map.sites[x].encodeAttributes());
	    flushReplay();
	}
    }
}
