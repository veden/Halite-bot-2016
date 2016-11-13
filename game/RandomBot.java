package game;

import java.util.ArrayList;
import java.util.Random;

public class RandomBot {
    public static void main(String[] args) throws java.io.IOException {
        InitPackage iPackage = Networking.getInit();
        int myID = iPackage.myID;
        GameMap gameMap = iPackage.map;

        Networking.sendInit("RandomJavaBot");

	Random r = new Random(12);
	
	while(true) {
	    ArrayList<Move> moves = new ArrayList<Move>();

	    gameMap = Networking.getFrame();

	    for(int y = 0; y < gameMap.height; y++) {
		for(int x = 0; x < gameMap.width; x++) {
		    boolean moved = false;
		    Site site = gameMap.getSite(new Location(x, y));
		    if (site.owner == myID) {
			for (Direction d : Direction.CARDINALS) {
			    Site attack = gameMap.getSite(new Location(x, y), d);
			    if ((attack.owner != myID) && (attack.strength < site.strength)) {
				moves.add(new Move(new Location(x, y), d));
				moved = true;
			    }
			}
			if ((!moved) && (site.strength > site.production * 5)) {
			    Direction dir = r.nextDouble() < 0.5 ? Direction.NORTH : Direction.WEST;
			    Site movement = gameMap.getSite(new Location(x, y), dir);
			    if ((movement.owner == myID) || (site.strength > movement.strength))
				moves.add(new Move(new Location(x, y), dir));
			}
		    }
		}
	    }
	    Networking.sendFrame(moves);
	}
    }
}
