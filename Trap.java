
import java.util.ArrayList;
import java.util.function.Predicate;


public class Trap {
    
    private ArrayList<UnitDirectionPair> layer1;
    private ArrayList<UnitDirectionPair> layer2;
    private ArrayList<UnitDirectionPair> layer3;

    public final ArrayList<Move> moves;
    
    public Trap(Map map, Tile target) {
	moves = new ArrayList<Move>();
	layer1 = new ArrayList<UnitDirectionPair>();
	layer2 = new ArrayList<UnitDirectionPair>();
	layer3 = new ArrayList<UnitDirectionPair>();

	final ArrayList<UnitDirectionPair> temp = new ArrayList<UnitDirectionPair>();
	
	ProcessTile getNeighbors = new ProcessTile() {
		@Override
		public void process(Map map, Tile tile) {
		    if (tile.mine())
			temp.add(new UnitDirectionPair(tile, map.directionFromTileToTile(tile, target)));
		}
	    };
	
	map.processCardinal(target.x, target.y, getNeighbors);
        layer1.addAll(temp);
	temp.clear();
	for (UnitDirectionPair up : layer1)
	    map.processCardinal(up.tile.x, up.tile.y, getNeighbors);
	layer2.addAll(temp);
	temp.clear();
	for (UnitDirectionPair up : layer2)
	    map.processCardinal(up.tile.x, up.tile.y, getNeighbors);
	layer3.addAll(temp);
	temp.clear();

	// MyBot.printLn(layer1.toString());
	// MyBot.printLn(layer2.toString());
	// MyBot.printLn(layer3.toString());
	// MyBot.printLn("-------");
	// MyBot.flush();
	
	boolean moved = false;
	for (int i = 0; i < layer1.size() && !moved; i++) {
	    UnitDirectionPair upI = layer1.get(i);
	    if (Troops.validUnitDirectionPairs(true, upI) && (upI.tile.getUnits() > target.getUnits())) {
		moves.add(Troops.unitDirectionToMove(upI));
		moved = true;
	    }
	}

	for (int i = 0; i < layer1.size() && !moved; i++) {
	    UnitDirectionPair upI = layer1.get(i);
	    for (int x = i+1; x < layer1.size() && !moved; x++) {
		UnitDirectionPair upX = layer1.get(x);
		if (Troops.validUnitDirectionPairs(true, upI, upX) && (Troops.sumTileAndReinforcementStrength(null, upI, upX) > target.getUnits())) {
		    moves.add(Troops.unitDirectionToMove(upI));
		    moves.add(Troops.unitDirectionToMove(upX));
		    moved = true;
		}
	    }
	}

	for (int i = 0; i < layer1.size() && !moved; i++) {
	    UnitDirectionPair upI = layer1.get(i);
	    for (int x = i+1; x < layer1.size() && !moved; x++) {
		UnitDirectionPair upX = layer1.get(x);
		for (int y = x+1; y < layer1.size() && !moved; y++) {
		    UnitDirectionPair upY = layer1.get(y);
		    if (Troops.validUnitDirectionPairs(true, upI, upX, upY) && (Troops.sumTileAndReinforcementStrength(null, upI, upX, upY) > target.getUnits())) {
			moves.add(Troops.unitDirectionToMove(upI));
			moves.add(Troops.unitDirectionToMove(upX));
			moves.add(Troops.unitDirectionToMove(upY));
			moved = true;
		    }
		}
	    }
	}

	//TODO: all sides
    }
}
