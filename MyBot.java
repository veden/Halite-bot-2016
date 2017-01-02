import game.Debug;
import game.GameMap;
import game.Networking;

import logic.Parameters;

public class MyBot {
    private static int turn = 0;
   
    public static void main(String[] args) throws java.io.IOException {
	long start = 0;
	GameMap map = new GameMap();
	Networking server = new Networking(map); 

	Debug.startup(map);
	server.sendInit("VedenV10");

	if (args.length > 0) {
	    Parameters.generatorWeight = Float.parseFloat(args[0]);
	    Parameters.siteCountWeight = Float.parseFloat(args[1]);
	    Parameters.sitePotentialWeight = Float.parseFloat(args[2]);
	    Parameters.siteCountWeight = Float.parseFloat(args[3]);
	    Parameters.generatorTotalWeight = Float.parseFloat(args[4]);
	}

	while(true) {
	    start = Debug.startClock(turn);
	    map.reset();
	    server.getFrame();
	    
	    map.scoreUnexplored();
	    map.identifyEnemy();
	    map.bot.planTroopMovements();
	    map.bot.move();

	    server.sendFrame();

	    Debug.stopClock(map, turn, start);
	    // if (turn == 125)
	    //Debug.abort("");
	    turn++;
	}
    }
}
