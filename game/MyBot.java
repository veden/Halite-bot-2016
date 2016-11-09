package game;

import java.io.FileNotFoundException;
import java.io.PrintWriter;

import logic.AI;

public class MyBot {
    private static PrintWriter pw;
    private static int frames = 0;

    public static void printLn(String s) {
	pw.println(s);
	pw.flush();
    }
    
    public static void main(String[] args) throws java.io.IOException {
	pw = new PrintWriter("debug.txt");
	try {
	    InitPackage iPackage = Networking.getInit();
	    int myID = iPackage.myID;
	    GameMap gameMap = iPackage.map;

	    printLn("starting AI");

	    AI p = new AI(gameMap);

	    printLn("starting game");

	    Networking.sendInit("VedenV1");
	    
	    while(true) {
		long start = System.currentTimeMillis();
		printLn("round - " + frames++);
		gameMap = Networking.getFrame();

		p.refreshTiles(gameMap);
	    
		Networking.sendFrame(p.process());
		printLn((System.currentTimeMillis() - start) + "-duration, " + frames + "-round");
	    }
	} finally {
	    pw.close();
	}
    }
}
