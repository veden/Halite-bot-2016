import java.util.ArrayList;
import java.util.Collections;

public class AnalysisProcess implements ProcessTile {

    public ArrayList<Tile> rankedOwnedTiles = new ArrayList<Tile>();
    public ArrayList<Tile> rankedFrontTiles = new ArrayList<Tile>();

    private OpeningProcess op = new OpeningProcess();
    
    public AnalysisProcess() {}
    
    public void reset() {
	rankedFrontTiles.clear();
	rankedOwnedTiles.clear();
    }

    public void postProcess() {
	Collections.sort(rankedOwnedTiles);
	Collections.sort(rankedFrontTiles);
    }
    
    @Override
    public void process(Map map, Tile tile) {
	op.reset();
	map.processCardinal(tile.x, tile.y, op);
	if (tile.mine()) {
	    rankedOwnedTiles.add(tile);
	    tile.openings = 4 - op.getCount();
	} else {
	    tile.ambushes = op.getCount();
	    if (tile.ambushes > 0)
		rankedFrontTiles.add(tile);
	}
    }
}
