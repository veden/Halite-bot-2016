package logic.unit;

import java.util.ArrayList;
import java.util.Collections;

import game.MyBot;

import logic.world.MapUtils;
import logic.world.Tile;

public class Trap {

    public Tile target;
    
    public ArrayList<UnitDirectionPair> ambushers;
    
    public Trap(APC apc, ArrayList<Tile> neighbors) {
	ambushers = new ArrayList<UnitDirectionPair>();
	
	target = apc.target;
	for (int i = 0; i < neighbors.size(); i++) {
	    Tile neighbor = neighbors.get(i);
	    if (neighbor.getOwner() == MyBot.ID)
		ambushers.add(new UnitDirectionPair(neighbor, MapUtils.toInverseDirection(i+1)));
	}
	Collections.sort(ambushers);
    }
}
