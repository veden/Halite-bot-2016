package logic.process;

import java.util.ArrayList;

import game.MyBot;

import logic.constant.Constants;
import logic.structure.Map;
import logic.structure.Tile;

public class DefenseProcess implements ProcessTile {

    public void process(Tile tile, ArrayList<Tile> neighbors) {
	float generator = tile.getStrengthGenerator() * Constants.GAME_SCALING;
	if (tile.getOwner() != MyBot.ID) {
	    tile.defense = generator;
	    tile.strength = 0;
	}
    }
}
