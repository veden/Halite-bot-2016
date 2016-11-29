import java.io.PrintWriter;
import java.util.Random;

public class MyBot {
    private static PrintWriter pw;
    private static PrintWriter r;
    public static int ID;
    public static PreComputed tables;
    public static int frames = 0;
    public static Random random = new Random(123);
    public static GameMap map;
    public static AI ai;

    public static void printLn(String s) {
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
    
    public static void main(String[] args) throws java.io.IOException {
	pw = new PrintWriter("debug.txt");
	r = new PrintWriter("replay.txt");
	try {
	    InitPackage iPackage = Networking.getInit();
	    ID = iPackage.myID;
	    map = iPackage.map;
	    printLn("starting AI");
	    tables = new PreComputed(map.dispersalReach);
	    
	    for (int i = 0; i < 15; i++) 
		map.refresh();
	    
	    Troops troops = new Troops();

	    printReplay(map.width + " " + map.height);
	    printLn("starting game");
	    
	    Networking.sendInit("VedenV4");
	    
	    while(true) {
		printLn("round - " + frames);
		printReplay("----");
		printReplay(frames+"");
		long start = System.currentTimeMillis();
		ai.clear();
		map = Networking.getFrame();
		map.refresh();
		Networking.sendFrame(troops.makeMoves());
		for (int x = 0; x < map.width; x++)
		    for (int y = 0; y < map.height; y++)
			printReplay(map.getSite(x, y).encodeString());
		printLn((System.currentTimeMillis() - start) + "-duration, " + frames++ + "-round");
		printLn(((Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()) / (1024 * 1024)) + "MB-used memory");
		flush();
		flushReplay();
	    }
	} finally {
	    pw.close();
	    r.close();
	}
    }
}
