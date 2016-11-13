package logic.unit;

import java.util.ArrayList;
import java.util.Collections;

import game.Direction;
import game.MyBot;

import logic.world.MapUtils;
import logic.world.Tile;

public class APC {
    
    public Tile target;
    public Direction targetDirection;
    
    public ArrayList<UnitDirectionPair> reinforcements;
    
    public APC(Tile position, ArrayList<Tile> neighbors, float productionThreshold) {
	target = null;
	reinforcements = new ArrayList<UnitDirectionPair>();

	targetDirection = Direction.STILL;

	for (int i = 0; i < neighbors.size(); i++) {
	    Tile neighbor = neighbors.get(i);
	    if ((neighbor.getOwner() != MyBot.ID) && neighbor.production >= productionThreshold) {
		if ((target == null) || ((target.production - target.defense <= neighbor.production - neighbor.defense))) {
		    target = neighbor;
		    targetDirection = MapUtils.toDirection(i+1);
		}
	    } else if (neighbor.getOwner() == MyBot.ID)
		reinforcements.add(new UnitDirectionPair(neighbor, MapUtils.toInverseDirection(i+1)));
	}
	Collections.sort(reinforcements);
    }
}
