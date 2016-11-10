package logic.unit;

import java.util.ArrayList;

import game.Move;

import logic.structure.Map;
import logic.structure.Tile;

public class Troops {

    private ArrayList<Worm> units;
    
    public Troops() {
	units = new ArrayList<Worm>();
    }

    public int size() {
	return units.size();
    }
    
    public ArrayList<Move> makeMoves(Map map) {
	ArrayList<Move> moves = new ArrayList<Move>();
	for (int i = 0; i < units.size(); i++) {
	    Move m = units.get(i).getMove(map);
	    if (m != null)
		moves.add(m);
	}
	return moves;
    }

    public void addWorm(Tile tile) {
	units.add(new Worm(tile));
    }
}
