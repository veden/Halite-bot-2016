package logic.world;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

import game.GameMap;
import game.MyBot;
import game.Site;

import logic.Constants;
import logic.process.DefenseProcess;
import logic.process.DispersalProcess;
import logic.process.ProcessSite;
import logic.process.ProcessTile;
import logic.process.ProductionProcess;
import logic.process.RankProcess;
import logic.process.StrengthProcess;

public class Map {

    private HashMap<Integer, HashMap<Integer, Tile>> tiles;
    private ArrayList<Tile> rankedOwnedTiles;
    private ArrayList<Tile> rankedFrontTiles;

    private ArrayList<Tile> neighbors;

    private ArrayList<ProcessTile> tileProcessors;

    private ProcessSite siteUpdater;
    private RankProcess rankProcess;
    
    public Map(GameMap gameMap) {
	tiles = new HashMap<Integer, HashMap<Integer, Tile>>();
	rankedOwnedTiles = new ArrayList<Tile>();
	rankedFrontTiles = new ArrayList<Tile>();
	rankProcess = new RankProcess(rankedOwnedTiles, rankedFrontTiles);
	siteUpdater = new ProcessSite();
	
	tileProcessors = new ArrayList<ProcessTile>();
	tileProcessors.add(new ProductionProcess());
	tileProcessors.add(new StrengthProcess());
	tileProcessors.add(new DefenseProcess());
	tileProcessors.add(new DispersalProcess());

	neighbors = new ArrayList<Tile>(4);
	for (int i = 0; i < 4; i++)
	    neighbors.add(null);

	refresh(gameMap, Constants.MAP_BURN_IN_ROUNDS);
    }

    public ArrayList<Tile> getRankedOwnedTiles() {
	return rankedOwnedTiles;
    }

    public ArrayList<Tile> getRankedFrontTiles() {
	return rankedFrontTiles;
    }
    
    public void refresh(GameMap gameMap, int burnInRounds) {
	MapUtils.processSites(gameMap, this, siteUpdater);
	for (int burnIn = 0; burnIn < burnInRounds; burnIn++)
	    MapUtils.processMap(this, tileProcessors);
	rankedOwnedTiles.clear();
	rankedFrontTiles.clear();
	MapUtils.processMap(this, rankProcess);
	Collections.sort(rankedOwnedTiles);
	Collections.sort(rankedFrontTiles);
	MyBot.printLn(rankedOwnedTiles.size()+"");
	for (int i = 0; i < rankedOwnedTiles.size(); i++)
	    MyBot.printLn(rankedOwnedTiles.get(i).toString());
	MyBot.printLn("--");
	MyBot.printLn(rankedFrontTiles.size()+"");
	for (int i = 0; i < rankedFrontTiles.size(); i++)
	    MyBot.printLn(rankedFrontTiles.get(i).toString());
	MyBot.printLn("--");
	for (Integer x : tiles.keySet())
	    for (Integer y : tiles.get(x).keySet()) {
		Tile t = tiles.get(x).get(y);
		MyBot.printLn(t.toString());
	    }
	MyBot.printLn("");
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
    
    public Tile updateTile(int x, int y, Site site) {
	HashMap<Integer, Tile> yMap = tiles.get(x);
	if (yMap == null) {
	    yMap = new HashMap<Integer, Tile>();
	    tiles.put(x, yMap);
	}

	Tile t = yMap.get(y);
	if (t != null)
	    t.updateTile(site);
	else {
	    t = new Tile(x, y, site);
	    yMap.put(y, t);
	}
	return t;
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
	neighbors.set(1, getTile(adjustedEast, y));
	neighbors.set(2, getTile(x, adjustedSouth));
	neighbors.set(3, getTile(adjustedWest, y));
	return neighbors;
    }
}
