public class DistributeProcess implements ProcessTile {

    private Tile center;
    private int maxRadius;
    private float generatePower;

    public DistributeProcess(int maxRadius) {
	this.maxRadius = maxRadius;
    }
    
    public void setCenter(Tile center) {
	this.center = center;
	generatePower = center.getGenerator() * (1 - (center.getUnits()/Constants.SITE_MAX_STRENGTH));
    }
    
    @Override
    public void process(Map map, Tile tile) {
	if (tile != center) {
	    float distance = map.manhattanDistance(center, tile);
	    if (distance <= maxRadius) {
		if (!center.mine()) {
		    if (!tile.mine()) {
			if (tile.getUnits() != 0)
			    tile.production += generatePower * (1/(distance)) * (1 - (tile.getUnits()/(Constants.SITE_MAX_STRENGTH*2)));
			else
			    tile.strength += generatePower * (1/(distance)); 
		    } else
			tile.strength += generatePower * (1/(distance));
		}
		// if (center.enemy())
		//     tile.strength += center.getUnits() * (1/distance);
	    }
	} else {
	    if (!center.mine()) {
		if (tile.getUnits() != 0)
		    tile.production += generatePower * (1 - (tile.getUnits()/Constants.SITE_MAX_STRENGTH));
		else
		    tile.strength += generatePower;
	    }
	    // if (center.enemy())
	    // 	tile.strength += center.getUnits();
	}
    }    
}
