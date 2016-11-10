package logic.process;

import java.util.ArrayList;

import game.MyBot;

import logic.structure.Tile;
import logic.unit.Troops;

public class FindWormProcess implements ProcessTile {

    private Troops troops;
    
    public FindWormProcess(Troops troops) {
	this.troops = troops;
    }
    
    public void process(Tile tile, ArrayList<Tile> neighbors) {
	if ((tile.getOwner() == MyBot.ID) && (troops.size() == 0))
	    troops.addWorm(tile);
    }
}
