package logic.process;

import java.util.ArrayList;

import game.GameMap;
import game.Location;

import logic.structure.Map;

public class ProcessUtils {

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
}
