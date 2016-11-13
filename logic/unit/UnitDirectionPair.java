package logic.unit;

import game.Direction;

import logic.world.Tile;

public class UnitDirectionPair implements Comparable<UnitDirectionPair> {
    public final Tile tile;
    public final Direction direction; 
	
    public UnitDirectionPair(Tile tile, Direction direction) {
	this.tile = tile;
	this.direction = direction;
    }

    @Override
    public int compareTo(UnitDirectionPair o) {
	int v = this.tile.getProductionGenerator() - o.tile.getProductionGenerator();
	if (v == 0)
	    return this.tile.id - o.tile.id;
	return v;
    } 
}
