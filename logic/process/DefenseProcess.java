package logic.process;

import java.util.ArrayList;

import game.MyBot;

import logic.Constants;
import logic.world.Tile;

public class DefenseProcess implements ProcessTile {

    public void process(Tile tile, ArrayList<Tile> neighbors) {
	if (tile.getOwner() != MyBot.ID)
	    tile.defense = tile.getStrengthGenerator() * Constants.GAME_SCALING;
    }
}
