package game;

import java.io.PrintWriter;

import logic.Constants;
import logic.unit.Troops;
import logic.world.Map;

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
	    printLn("starting AI");

	    Map map = new Map(iPackage.map);
	    Troops troops = new Troops();
	    
	    printLn("starting game");

	    Networking.sendInit("VedenV1");
	    
	    while(true) {
		long start = System.currentTimeMillis();
		printLn("round - " + frames++);
		map.refresh(Networking.getFrame(), Constants.MAP_LIGHT_BURN_IN_ROUNDS);
		Networking.sendFrame(troops.makeMoves(map));
		printLn((System.currentTimeMillis() - start) + "-duration, " + frames + "-round");
		flush();
	    }
	} finally {
	    pw.close();
	}
    }
}
