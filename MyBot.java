import java.io.PrintWriter;
import java.util.Random;

import game.GameMap;
import game.Networking;
import game.Stats;
import game.bot.AI;
import game.bot.model.Enemy;

public class MyBot {

    private boolean debug = true;
          
    private PrintWriter pw;
    private PrintWriter r;

    private short round = 0;

    public Random random = new Random(1233);
    
    public void println(String s) {
	pw.println(s);
    }

    public void printReplay(String s) {
	r.println(s);
    }

    public void flushReplay() {
	r.flush();
    }
    
    public void flush() {
	pw.flush();
    }

    public void abort(String message) {
	println(message);
	flush();
	throw new RuntimeException();
    }
    
    public void main(String[] args) throws java.io.IOException {
	if (debug) {
	    pw = new PrintWriter("debug.txt");
	    r = new PrintWriter("replay.txt");
	}
	GameMap map = new GameMap();
	AI ai = new AI();
	Networking server = new Networking(map, ai);
	    
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

	    Enemy.analyze();
	    ai.analyze();
	    Enemy.postAnalyze();
	    ai.postAnalyze();
	
	    ai.move();

	    if (debug) {
		map.collectStats();

		println((System.currentTimeMillis() - start) + "-duration");
		println(((Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / (1024 * 1024)) + "MB-used memory");

		flush();
	
		printReplay("----");
		printReplay(round + " " + Stats.minExplore + " " + Stats.maxExplore + " " + Stats.minStrength + " " + Stats.maxStrength + " " + Stats.minDamage + " " + Stats.maxDamage + " " + Stats.minDefense + " " + Stats.maxDefense + " " + Stats.minStrategy + " " + Stats.maxStrategy + " " + Stats.totalUnexploredGenerator);
		printReplay("===");
		printReplay(ai.toString());
		printReplay(Enemy.toReplay());
		printReplay("===");
		for (int x = 0; x < map.width; x++)
		    for (int y = 0; y < map.height; y++)
			printReplay(map.getSite(x, y).encodeString());
		flushReplay();
	    }
	    //	abort("");
	}
    }
}
