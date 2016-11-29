import java.util.ArrayList;

public class APC {
    
    public Tile target;
    public final Direction targetDirection;
    
    public ArrayList<UnitDirectionPair> reinforcements;
    
    public APC(Map map, Tile position, float productionThreshold) {
	target = position;
	map.processCardinal(position.x, position.y, new ProcessTile() {
		@Override
		public void process(Map map, Tile tile) {
		    if (tile.mine())
			if (target.strength < tile.strength)
			    target = tile;
		}
	    });

	if (target == position)
	    map.processCardinal(position.x, position.y, new ProcessTile() {
		    @Override
		    public void process(Map map, Tile tile) {
			if (!tile.mine())
			    if ((target.production < tile.production) && (position.getUnits() > tile.getUnits()))
				target = tile;
		    }
		});
	
	targetDirection = map.directionFromTileToTile(position, target);
    }
}
