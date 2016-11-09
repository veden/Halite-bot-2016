package logic;

import java.util.ArrayList;

import game.GameMap;
import game.Location;
import game.Move;
import game.MyBot;

import logic.structure.Map;

public class AI {

    private Map map;

    
    public AI(GameMap gameMap) {
	map = new Map();
	refreshTiles(gameMap);
    }

    public void refreshTiles(GameMap gameMap) {
	Location l = new Location(0, 0);
	for (int x = 0; x < gameMap.width; x++)
	    for (int y = 0; y < gameMap.height; y++) {
		l.x = x;
		l.y = y;
		map.updateTile(x, y, gameMap.getSite(l));
	    }

	MyBot.printLn("updated all tiles");
	
	for (int x = 0; x < gameMap.width; x++)
	    for (int y = 0; y < gameMap.height; y++) {
		map.getNeighbors(x, y);
	    }
    }
    
    public ArrayList<Move> process() {
	ArrayList<Move> moves = new ArrayList<Move>();

	return moves;
    }
}
