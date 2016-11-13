package logic.process;

import java.util.ArrayList;

import game.MyBot;

import logic.Constants;
import logic.world.Tile;

public class StrengthProcess implements ProcessTile {

    public void process(Tile tile, ArrayList<Tile> neighbors) {
	float generator = tile.getStrengthGenerator() * Constants.GAME_SCALING;
	if (tile.getOwner() == MyBot.ID)
	    tile.strength = generator;
    }
}
