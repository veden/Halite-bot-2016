import game.Debug;
import game.GameMap;
import game.Networking;

import logic.Parameters;

public class MyBot {
    private static int turn = 0;
   
    public static void main(String[] args) throws java.io.IOException {
	if (args.length > 0) {
	    Parameters.generatorWeight = Float.parseFloat(args[0]);
	    Parameters.siteCountWeight = Float.parseFloat(args[1]);
	    Parameters.sitePotentialWeight = Float.parseFloat(args[2]);
	    Parameters.siteCountWeight = Float.parseFloat(args[3]);
	    Parameters.generatorTotalWeight = Float.parseFloat(args[4]);
	    // Parameters.level1Defense = Float.parseFloat(args[5]);
	    // Parameters.level2Defense = Float.parseFloat(args[6]);
	    // Parameters.level3Defense = Float.parseFloat(args[7]);
	    // Parameters.phaseLevel2 = Float.parseFloat(args[8]);
	    // Parameters.phaseLevel3 = Float.parseFloat(args[9]);
	    Parameters.objectiveThreshold = Float.parseFloat(args[10]);
	    Parameters.objectiveUnitSpread = Float.parseFloat(args[11]);
	    Parameters.objectiveGeneratorSpread = Float.parseFloat(args[12]);
	    Parameters.reinforceSpread = Float.parseFloat(args[13]);
	    Parameters.enemyGeneratorWeight = Float.parseFloat(args[14]);
	    Parameters.enemyUnitWeight = Float.parseFloat(args[15]);
	    Parameters.enemyGeneratorSpread = Float.parseFloat(args[16]);
	    Parameters.enemyUnitSpread = Float.parseFloat(args[17]);
	    // Parameters.bumpMultiplerThreshold = Float.parseFloat(args[18]);
	    // Parameters.evadeThreshold = Float.parseFloat(args[19]);
	    Parameters.sitePotentialWeighting = Float.parseFloat(args[20]);
	    Parameters.sitePotentialDistance = Float.parseFloat(args[21]);
	}

	long start = 0;
	GameMap map = new GameMap();
	Networking server = new Networking(map); 

	map.analyzeUnexplored();
	
	Debug.startup(map);
	server.sendInit("VedenV16");

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
	    // if (turn == 40)
	    // 	Debug.abort("");
	    turn++;
	}
    }
}
