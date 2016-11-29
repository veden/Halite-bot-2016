public class DispersalProcess implements ProcessTile {

    private DistributeProcess dp;
    private int maxRadius;
    
    public DispersalProcess(int maxRadius) {
	dp = new DistributeProcess(maxRadius);
	this.maxRadius = maxRadius;
    }
    
    @Override
    public void process(Map map, Tile tile) {
	dp.setCenter(tile);
	
	map.processRegion(tile.x-maxRadius,
			  tile.y-maxRadius,
			  tile.x+maxRadius,
			  tile.y+maxRadius,
			  dp);
    }
}
