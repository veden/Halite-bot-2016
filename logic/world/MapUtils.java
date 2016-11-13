package logic.world;

import java.util.ArrayList;

import game.Direction;
import game.GameMap;
import game.Location;
import game.MyBot;

import logic.process.ProcessSite;
import logic.process.ProcessTile;
import logic.world.Map;
import logic.world.Tile;

public class MapUtils {

    public static void processMap(Map map, ProcessTile pu) {
	for (int x = 0; x < map.getMapWidth(); x++)
	    for (int y = 0; y < map.getMapHeight(); y++)
		pu.process(map.getTile(x, y), map.getNeighbors(x, y));
    }
   
    public static void processMap(Map map, ArrayList<ProcessTile> pus) {
	for (int x = 0; x < map.getMapWidth(); x++)
	    for (int y = 0; y < map.getMapHeight(); y++)
		for (int i = 0; i < pus.size(); i++)
		    pus.get(i).process(map.getTile(x, y), map.getNeighbors(x, y));
    }

    public static void processSites(GameMap gameMap, Map map, ProcessSite ps) {
	Location l = new Location(0, 0);
	for (int x = 0; x < gameMap.width; x++)
	    for (int y = 0; y < gameMap.height; y++) {
		l.x = x;
		l.y = y;
		ps.process(map, x, y, gameMap.getSite(l));
	    }
    }

    public static Direction toDirection(int i) {
	if (i == 0)
	    return Direction.STILL;
	else if (i == 1)
	    return Direction.NORTH;
	else if (i == 2)
	    return Direction.EAST;
	else if (i == 3)
	    return Direction.SOUTH;
	else if (i == 4)
	    return Direction.WEST;
	return null;
    }
    
    public static Direction toInverseDirection(int i) {
	if (i == 0)
	    return Direction.STILL;
	else if (i == 1)
	    return Direction.SOUTH;
	else if (i == 2)
	    return Direction.WEST;
	else if (i == 3)
	    return Direction.NORTH;
	else if (i == 4)
	    return Direction.EAST;
	return null;
    }
    
    public static void calculateOpenings(Tile t, ArrayList<Tile> neighbors) {
	int openings = 0;
	for (int i = 0; i < neighbors.size(); i++)
	    if (neighbors.get(i).getOwner() != MyBot.ID)
		openings++;
	t.openings = openings;
    }

    public static void calculateAmbushes(Tile t, ArrayList<Tile> neighbors) {
	int ambushes = 0;
	for (int i = 0; i < neighbors.size(); i++)
	    if (neighbors.get(i).getOwner() == MyBot.ID)
		ambushes++;
	t.ambushes = ambushes;
    }
}
