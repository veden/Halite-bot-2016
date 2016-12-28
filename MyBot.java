import game.Debug;
import game.GameMap;
import game.Networking;

public class MyBot {
    private static int turn = 0;
   
    public static void main(String[] args) throws java.io.IOException {
	long start = 0;
	GameMap map = new GameMap();
	Networking server = new Networking(map); 

	Debug.startup(map);
	server.sendInit("VedenV7");

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
