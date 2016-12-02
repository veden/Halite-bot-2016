package game;

import java.io.FileNotFoundException;
import java.io.PrintWriter;

import game.bot.Bot;
import game.bot.model.Enemy;

public class Harness {

    private static boolean debug = true;
        
    public static Bot bot;
    public static GameMap map;
    private static Networking server;
    
    private static PrintWriter pw;
    private static PrintWriter r;

    private static short round = 0;
    
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
    
    public static void startBot() throws FileNotFoundException {
	if (debug) {
	    pw = new PrintWriter("debug.txt");
	    r = new PrintWriter("replay.txt");
	}
	map = new GameMap();
	server = new Networking();
	    
	if (debug) {
	    println("starting AI");
	    long start = System.currentTimeMillis();
	    printReplay(map.width + " " + map.height);
	    println((System.currentTimeMillis() - start) + "-start up");
	    println(((Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / (1024 * 1024)) + "MB-used memory");
	    flush();
	}
	server.sendInit("VedenV4");
    }

    public static void readMap() {
	long start = 0;
	if (debug) {
	    println("Frame-" + round++);
	    start = System.currentTimeMillis();
	}

	map.reset();
	bot.reset();
	Enemy.resetAll();
	
	server.getFrame();

	Enemy.analyze();
	bot.analyze();
	Enemy.postAnalyze();
	bot.postAnalyze();
	
	bot.move();
	
	if (debug) {
	    println((System.currentTimeMillis() - start) + "-duration");
	    println(((Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / (1024 * 1024)) + "MB-used memory");

	    flush();
	
	    printReplay("----");
	    printReplay(round+"");
	    for (int x = 0; x < map.width; x++)
		for (int y = 0; y < map.height; y++)
		    printReplay(map.getSite(x, y).encodeString());
	    flushReplay();
	}
    }

    public static void sendMoves() {
	server.sendFrame();
    }
}
