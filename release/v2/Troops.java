import java.util.ArrayList;

public class Troops {
    
    public ArrayList<Move> makeMoves(Map map) {
	ArrayList<Move> moves = new ArrayList<Move>();
	ArrayList<Tile> rankedFrontTiles = map.getFrontTiles();

	float productionThreshold = 0.0f;	
	for (int i = 0; i < rankedFrontTiles.size(); i++)
	    productionThreshold += rankedFrontTiles.get(i).production;
	productionThreshold /= rankedFrontTiles.size();

	for (int i = 0; i < rankedFrontTiles.size(); i++)
	    pullToPoint(rankedFrontTiles.get(i),
			productionThreshold,
			map,
			moves);
	
	ArrayList<Tile> rankedOwnedTiles = map.getOwnedTiles();
	for (int i = 0; i < rankedOwnedTiles.size(); i++)
	    movesFromPoint(rankedOwnedTiles.get(i),
			   productionThreshold,
			   map,
			   moves);
	return moves;
    }

    private void pullToPoint(Tile tile, float productionThreshold, Map map, ArrayList<Move> moves) {
	if ((tile.production >= productionThreshold) || (tile.production == 0)) {
	    Trap trap = new Trap(map, tile);
	    for (Move m : trap.moves)
		moves.add(m);
	}
    }

    private void movesFromPoint(Tile tile, float productionThreshold, Map map, ArrayList<Move> moves) {
	if (tile.aboveUnitThreshold() && !tile.used) {
	    APC apc = new APC(map, tile, productionThreshold);
	    if (apc.target != null)
		moves.add(tileToMove(tile, apc.targetDirection));
	}	
    }

    public static boolean validUnitDirectionPairs(boolean checkStrength, UnitDirectionPair... ups) {
	for (int i = 0; i < ups.length; i++) {
	    UnitDirectionPair up = ups[i]; 
	    if (!(up != null && !up.tile.used && ((checkStrength && up.tile.aboveUnitThreshold()) || !checkStrength)))
		return false;
	}
	return true;
    }

    public static int sumTileAndReinforcementStrength(Tile t, UnitDirectionPair... ups) {
	int value = 0;
	for (int i = 0; i < ups.length; i++)
	    value += ups[i].tile.getUnits();
	if (t != null)
	    return value + t.getUnits();
	else
	    return value;
    }

    public static Move unitDirectionToMove(UnitDirectionPair up) {
	up.tile.used = true;
	return new Move(new Location(up.tile.x, up.tile.y), up.direction);
    }

    public static Move tileToMove(Tile t, Direction d) {
	t.used = true;
	return new Move(new Location(t.x, t.y), d);
    }
}
