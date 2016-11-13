package logic.process;

import java.util.ArrayList;

import game.MyBot;

import logic.world.Tile;

public class ProductionProcess implements ProcessTile {

    public void process(Tile tile, ArrayList<Tile> neighbors) {
	if (tile.getOwner() != MyBot.ID)
	    tile.production = tile.getProductionGenerator() * Tile.getProductionScaling();
    }
}
