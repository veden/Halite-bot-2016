package logic.unit;

import java.util.ArrayList;

import game.Direction;
import game.Location;
import game.Move;

import logic.world.Map;
import logic.world.MapUtils;
import logic.world.Tile;

public class Troops {
    
    public ArrayList<Move> makeMoves(Map map) {
	ArrayList<Move> moves = new ArrayList<Move>();
	ArrayList<Tile> rankedFrontTiles = map.getRankedFrontTiles();

	float productionThreshold = 0.0f;
	
	for (int i = 0; i < rankedFrontTiles.size(); i++) {
	    Tile front = rankedFrontTiles.get(i);
	    productionThreshold += front.production;
	}
	if (rankedFrontTiles.size() != 0)
	    productionThreshold /= rankedFrontTiles.size();
	else
	    productionThreshold = -1.0f;
	
	ArrayList<Tile> rankedOwnedTiles = map.getRankedOwnedTiles();
	for (int i = 0; i < rankedOwnedTiles.size(); i++) {
	    Tile t = rankedOwnedTiles.get(i);
	    movesFromPoint(t, productionThreshold, map, moves);
	}
	return moves;
    }

    private void movesFromPoint(Tile tile, float productionThreshold, Map map, ArrayList<Move> moves) {
	if (!tile.used) {
	    if (tile.openings > 0) {
		APC apc = null;
		UnitDirectionPair ally1 = null;
		UnitDirectionPair ally2 = null;
		UnitDirectionPair ally3 = null;
		apc = new APC(tile, map.getNeighbors(tile.x, tile.y), productionThreshold);
		if (tile.openings == 3)
		    ally1 = apc.reinforcements.get(0);
		else if (tile.openings == 2) {
		    ally1 = apc.reinforcements.get(0);
		    ally2 = apc.reinforcements.get(1);
		} else if (tile.openings == 1) {
		    ally1 = apc.reinforcements.get(0);
		    ally2 = apc.reinforcements.get(1);
		    ally3 = apc.reinforcements.get(2);
		}
		if ((apc.target != null) && !tile.used) {
		    //calculateAmbushes(tile, new Trap(apc, map.getNeighbors(apc.target.x, apc.target.y)), moves);
		    calculateOpenings(tile, apc, ally1, ally2, ally3, moves);
		}
	    } else {
		if (tile.aboveStrengthThreshold()) {
		    Tile target = null;
		    Direction targetDirection = Direction.STILL;
		    ArrayList<Tile> neighbors = map.getNeighbors(tile.x, tile.y);
		    for (int i = 0; i < neighbors.size(); i++) {
			Tile neighbor = neighbors.get(i);
			if ((target == null) || (target.production < neighbor.production)) {
			    targetDirection = MapUtils.toDirection(i+1);
			    target = neighbor;
			}
		    }
		    if (target != null)
			moves.add(tileToMove(tile, targetDirection));
		}
	     }
	 }
    }

    private boolean validUnitDirectionPairs(boolean checkStrength, UnitDirectionPair... ups) {
	for (int i = 0; i < ups.length; i++) {
	    UnitDirectionPair up = ups[i];
	    if (!(up != null && !up.tile.used && ((checkStrength && up.tile.aboveStrengthThreshold()) || !checkStrength)))
		return false;
	}
	return true;
    }

    private int sumTileAndReinforcementStrength(Tile t, UnitDirectionPair... ups) {
	int value = 0;
	for (int i = 0; i < ups.length; i++)
	    value += ups[i].tile.getStrengthGenerator();
	if (t != null)
	    return value + t.getStrengthGenerator();
	else
	    return value;
    }

    private Move unitDirectionToMove(UnitDirectionPair up) {
	up.tile.used = true;
	return new Move(new Location(up.tile.x, up.tile.y), up.direction);
    }

    private Move tileToMove(Tile t, Direction d) {
	t.used = true;
	return new Move(new Location(t.x, t.y), d);
    }

    public void calculateOpenings(Tile tile, APC apc, UnitDirectionPair r1, UnitDirectionPair r2, UnitDirectionPair r3, ArrayList<Move> moves) {
	if (tile.aboveStrengthThreshold() && (apc.target.getStrengthGenerator() < tile.getStrengthGenerator()))
	    moves.add(tileToMove(tile, apc.targetDirection));

	// if ((!tile.used) && (tile.aboveStrengthThreshold())) {
	//     if (validUnitDirectionPairs(false, r1) && (apc.target.production < r1.tile.production))
	// 	moves.add(tileToMove(tile, MapUtils.toInverseDirection(r1.direction.ordinal())));
	//     else if (validUnitDirectionPairs(false, r2) && (apc.target.production < r2.tile.production))
	// 	moves.add(tileToMove(tile, MapUtils.toInverseDirection(r2.direction.ordinal())));
	//     else if (validUnitDirectionPairs(false, r3) && (apc.target.production < r3.tile.production))
	// 	moves.add(tileToMove(tile, MapUtils.toInverseDirection(r3.direction.ordinal())));
	// }

	if ((!tile.used) && (tile.getStrengthGenerator() != 0)) {
	    if (validUnitDirectionPairs(true, r1) && (sumTileAndReinforcementStrength(tile, r1) > apc.target.getStrengthGenerator()))
		moves.add(unitDirectionToMove(r1));
	    else if (validUnitDirectionPairs(true, r2) && (sumTileAndReinforcementStrength(tile, r2) > apc.target.getStrengthGenerator()))
		moves.add(unitDirectionToMove(r2));
	    else if (validUnitDirectionPairs(true, r3) && (sumTileAndReinforcementStrength(tile, r3) > apc.target.getStrengthGenerator()))
		moves.add(unitDirectionToMove(r3));
	    else if (validUnitDirectionPairs(true, r1, r2) && (sumTileAndReinforcementStrength(tile, r1, r2) > apc.target.getStrengthGenerator())) {
		moves.add(unitDirectionToMove(r1));
		moves.add(unitDirectionToMove(r2));
	    } else if (validUnitDirectionPairs(true, r1, r3) && (sumTileAndReinforcementStrength(tile, r1, r3) > apc.target.getStrengthGenerator())) {
		moves.add(unitDirectionToMove(r1));
		moves.add(unitDirectionToMove(r3));
	    } else if (validUnitDirectionPairs(true, r2, r3) && (sumTileAndReinforcementStrength(tile, r2, r3) > apc.target.getStrengthGenerator())) {
		moves.add(unitDirectionToMove(r2));
		moves.add(unitDirectionToMove(r3));
	    }  else if (validUnitDirectionPairs(true, r1, r2, r3) && (sumTileAndReinforcementStrength(tile, r1, r2, r3) > apc.target.getStrengthGenerator())) {
		moves.add(unitDirectionToMove(r1));
		moves.add(unitDirectionToMove(r2));
		moves.add(unitDirectionToMove(r3));
	    }
	}
    }
    
    public void calculateAmbushes(Tile tile, Trap trap, ArrayList<Move> moves) {
	UnitDirectionPair ambusher1 = null;
	UnitDirectionPair ambusher2 = null;
	UnitDirectionPair ambusher3 = null;
	UnitDirectionPair ambusher4 = null;
	if (trap.target.ambushes == 2) {
	    ambusher1 = trap.ambushers.get(0);
	    ambusher2 = trap.ambushers.get(1);
	} else if (trap.target.ambushes == 3) {
	    ambusher1 = trap.ambushers.get(0);
	    ambusher2 = trap.ambushers.get(1);
	    ambusher3 = trap.ambushers.get(2);
	} else if (trap.target.ambushes == 4) {
	    ambusher1 = trap.ambushers.get(0);
	    ambusher2 = trap.ambushers.get(1);
	    ambusher3 = trap.ambushers.get(2);
	    ambusher4 = trap.ambushers.get(3);
	}

	if (validUnitDirectionPairs(true, ambusher1, ambusher2) && (sumTileAndReinforcementStrength(null, ambusher1, ambusher2) > trap.target.getStrengthGenerator())) {
	    moves.add(unitDirectionToMove(ambusher1));
	    moves.add(unitDirectionToMove(ambusher2));
	} else if (validUnitDirectionPairs(true, ambusher1, ambusher2, ambusher3) && (sumTileAndReinforcementStrength(null, ambusher1, ambusher2, ambusher3) > trap.target.getStrengthGenerator())) {
	    moves.add(unitDirectionToMove(ambusher1));
	    moves.add(unitDirectionToMove(ambusher2));
	    moves.add(unitDirectionToMove(ambusher3));
	} else if (validUnitDirectionPairs(true, ambusher1, ambusher2, ambusher3, ambusher4) && (sumTileAndReinforcementStrength(null, ambusher1, ambusher2, ambusher3, ambusher4) > trap.target.getStrengthGenerator())) {
	    moves.add(unitDirectionToMove(ambusher1));
	    moves.add(unitDirectionToMove(ambusher2));
	    moves.add(unitDirectionToMove(ambusher3));
	    moves.add(unitDirectionToMove(ambusher4));
	}
    }
}
