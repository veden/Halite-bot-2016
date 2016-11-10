package game;

import java.io.PrintWriter;

import logic.structure.Map;
import logic.unit.Troops;

public class MyBot {
    private static PrintWriter pw;
    public static int ID;
    private static int frames = 0;

    public static void printLn(String s) {
	pw.println(s);
    }

    public static void flush() {
	pw.flush();
    }
    
    public static void main(String[] args) throws java.io.IOException {
	pw = new PrintWriter("debug.txt");
	try {
	    InitPackage iPackage = Networking.getInit();
	    ID = iPackage.myID;
	    GameMap gameMap = iPackage.map;

	    printLn("starting AI");

	    Troops troops = new Troops();
	    Map map = new Map(gameMap, troops);
	    
	    printLn("starting game");

	    Networking.sendInit("VedenV1");
	    
	    while(true) {
		long start = System.currentTimeMillis();
		printLn("round - " + frames++);
		gameMap = Networking.getFrame();

		map.refresh(gameMap);
	    
		Networking.sendFrame(troops.makeMoves(map));
		printLn((System.currentTimeMillis() - start) + "-duration, " + frames + "-round");
		flush();
	    }
	} finally {
	    pw.close();
	}
    }
}
