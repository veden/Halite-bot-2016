package logic.process;

import java.util.ArrayList;

import game.MyBot;

import logic.world.MapUtils;
import logic.world.Tile;

public class RankProcess implements ProcessTile {

    private ArrayList<Tile> rankedOwnedTiles;
    private ArrayList<Tile> rankedFrontTiles;
    
    public RankProcess(ArrayList<Tile> rankedOwnedTiles, ArrayList<Tile> rankedFrontTiles ) {
	this.rankedOwnedTiles = rankedOwnedTiles;
	this.rankedFrontTiles = rankedFrontTiles;
    }
    
    public void process(Tile t, ArrayList<Tile> neighbors) {
	if (t.getOwner() == MyBot.ID) {
	    rankedOwnedTiles.add(t);
	    MapUtils.calculateOpenings(t, neighbors);
	} else {
	    MapUtils.calculateAmbushes(t, neighbors);
	    if (t.ambushes > 0)
		rankedFrontTiles.add(t);
	}	
    }    
}
