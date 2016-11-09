package logic.structure;

import java.util.ArrayList;
import java.util.HashMap;

import game.MyBot;
import game.Site;

public class Map {

    private HashMap<Integer, HashMap<Integer, Tile>> tiles;

    private ArrayList<Tile> neighbors;
    
    public Map() {
	tiles = new HashMap<Integer, HashMap<Integer, Tile>>();
	neighbors = new ArrayList<Tile>();
    }

    public Tile getTile(int x, int y) {
	return tiles.get(x).get(y);
    }

    public void updateTile(int x, int y, Site site) {
	HashMap<Integer, Tile> yMap = tiles.get(x);
	if (yMap == null) {
	    yMap = new HashMap<Integer, Tile>();
	    tiles.put(x, yMap);
	}

	Tile t = yMap.get(y);
	if (t != null)
	    t.updateTile(site);
	else
	    yMap.put(y, new Tile(x, y, site));
    }

    private int safeCoordinate(int x, int z) {
	int temp = x + z;
	if (temp < 0)
	    return tiles.size() - 1;
	else if (temp >= tiles.size())
	    return 0;
	else
	    return temp;
    }
    
    public ArrayList<Tile> getNeighbors(int x, int y) {
	int adjustedNorth = safeCoordinate(y, -1);
	int adjustedEast = safeCoordinate(x, 1);
	int adjustedWest = safeCoordinate(x, -1);
	int adjustedSouth = safeCoordinate(y, 1);
	neighbors.set(0, getTile(x, adjustedNorth));
	neighbors.set(1, getTile(adjustedWest, y));
	neighbors.set(2, getTile(adjustedEast, y));
	neighbors.set(3, getTile(x, adjustedSouth));
	return neighbors;
    }
}
