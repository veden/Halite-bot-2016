package logic.structure;

import java.util.ArrayList;
import java.util.HashMap;

import game.GameMap;
import game.Site;

import logic.constant.Constants;
import logic.process.DefenseProcess;
import logic.process.DispersalProcess;
import logic.process.FindWormProcess;
import logic.process.ProcessSite;
import logic.process.ProcessTile;
import logic.process.ProcessUtils;
import logic.process.ProductionProcess;
import logic.process.StrengthProcess;
import logic.unit.Troops;

public class Map {

    private HashMap<Integer, HashMap<Integer, Tile>> tiles;

    private ArrayList<Tile> neighbors;

    private ArrayList<ProcessTile> tileProcessors;
    private FindWormProcess findWormProcess;

    private ProcessSite siteUpdater;
    
    public Map(GameMap gameMap, Troops troops) {
	tiles = new HashMap<Integer, HashMap<Integer, Tile>>();
	siteUpdater = new ProcessSite();

	findWormProcess = new FindWormProcess(troops);
	
	tileProcessors = new ArrayList<ProcessTile>();
	tileProcessors.add(new ProductionProcess());
	tileProcessors.add(new StrengthProcess());
        tileProcessors.add(new DefenseProcess());
	tileProcessors.add(new DispersalProcess());

	neighbors = new ArrayList<Tile>(4);
	for (int i = 0; i < 4; i++)
	    neighbors.add(null);

	initGameMap(gameMap);
    }
    
    private void initGameMap(GameMap gameMap) {
	ProcessUtils.processSites(gameMap, this, siteUpdater);
	for (int burnIn = 0; burnIn < Constants.MAP_BURN_IN; burnIn++)
	    ProcessUtils.processMap(this, tileProcessors);
	ProcessUtils.processMap(this, findWormProcess);
    }

    public void refresh(GameMap gameMap) {
	ProcessUtils.processSites(gameMap, this, siteUpdater);
	for (int burnIn = 0; burnIn < 10; burnIn++)
	    ProcessUtils.processMap(this, tileProcessors); 
    }
    
    public int getMapWidth() {
	return tiles.size();
    }

    public int getMapHeight() {
	return tiles.get(0).size();
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
