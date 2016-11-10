package logic.unit;

import java.util.ArrayList;

import game.Direction;
import game.Location;
import game.Move;
import game.MyBot;

import logic.structure.Map;
import logic.structure.Tile;

public class Worm {

    private Tile head;
    
    public Worm(Tile head) {
	this.head = head;
    }

    public float scoreTile(Tile tile) {
	float score = 0.0f;
	if (tile.getOwner() == MyBot.ID) {
	    
	} else {
	    
	}
	float a = (tile.production);
	if (head.getStrengthGenerator() > tile.getStrengthGenerator())
	    a += tile.defense;
	else
	    a -= tile.defense;
	return score;
    }
    
    public Move getMove(Map map) {
	Tile currentTile = map.getTile(head.x, head.y);
	ArrayList<Tile> neighbors = map.getNeighbors(head.x, head.y);
	
	float highest = scoreTile(currentTile);
	Tile highTile = currentTile;
	Direction d = Direction.STILL;
	
	for (int i = 0; i < neighbors.size(); i++) {
	    Tile tile = neighbors.get(i);
	    float score = scoreTile(tile);
	    if (score > highest) {
		highTile = tile;
		highest = score;
		if (i == 0)
		    d = Direction.NORTH;
		else if (i == 1)
		    d = Direction.WEST;
		else if (i == 2)
		    d = Direction.EAST;
		else if (i == 3)
		    d = Direction.SOUTH;
	    }
	    MyBot.printLn(score + " - " + tile.x + ", " + tile.y);
	}
	Location l = new Location(head.x, head.y);
	head = highTile;
	MyBot.printLn(highest + " / " + highTile.x + ", " + highTile.y + " - " + d + " current " + currentTile.x + ", " + currentTile.y);
	return new Move(l, d);
    }
}
