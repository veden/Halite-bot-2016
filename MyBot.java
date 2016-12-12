import java.io.PrintWriter;
import java.util.Random;

import bot.AI;

import game.GameMap;
import game.Networking;
import game.Stats;

public class MyBot {

    private static boolean debug = true;
          
    private static PrintWriter pw;
    private static PrintWriter r;

    private static short round = 0;

    public Random random = new Random(1233);
    
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
    
    public static void main(String[] args) throws java.io.IOException {
	if (debug) {
	    pw = new PrintWriter("/home/veden/haliteFiles/debug.txt");
	    r = new PrintWriter("/home/veden/haliteFiles/replay.txt");
	}
	GameMap map = new GameMap();
	Networking server = new Networking(map);
        map.bot = new AI(server.myId, map);
	    
	if (debug) {
	    println("starting AI");
	    long start = System.currentTimeMillis();
	    printReplay(map.width + " " + map.height + " " + Stats.minGenerator + " " + Stats.maxGenerator + "--" + Stats.siteCounts());
	    println((System.currentTimeMillis() - start) + "-start up");
	    println(((Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / (1024 * 1024)) + "MB-used memory");
	    flush();
	}
	server.sendInit("VedenV7");
	
	while(true) {
	    long start = 0;
	    if (debug) {
		println("Frame-" + round++);
		start = System.currentTimeMillis();
	    }
	    server.getFrame();
	    map.commitChanges();

	    map.analyzeEnemies();
	    map.bot.analyze();
	    map.postAnalyzeEnemies();
	    map.bot.postAnalyze();
	
	    map.bot.move();

	    server.sendFrame();
	    
	    if (debug) {
		map.collectStats();

		println((System.currentTimeMillis() - start) + "-duration");
		println(((Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / (1024 * 1024)) + "MB-used memory");

		flush();
	
		printReplay("----");
		printReplay(round + " " + Stats.minExplore + " " + Stats.maxExplore + " " + Stats.minStrength + " " + Stats.maxStrength + " " + Stats.minDamage + " " + Stats.maxDamage + " " + Stats.minDefense + " " + Stats.maxDefense + " " + Stats.minStrategy + " " + Stats.maxStrategy + " " + Stats.totalUnexploredGenerator);
		printReplay("===");
		printReplay(map.bot.toString());
		printReplay(map.enemiesToReplay());
		printReplay("===");
		for (int x = 0; x < map.width; x++)
		    for (int y = 0; y < map.height; y++)
			printReplay(map.getSite(x, y).encodeString());
		flushReplay();
	    }
	    //	    abort("");
	}
    }
}
