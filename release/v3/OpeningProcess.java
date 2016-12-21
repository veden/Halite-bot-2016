public class OpeningProcess implements ProcessTile {

    private int count = 0;
    
    public void reset() {
        count = 0;
    }

    public int getCount() {
	return count;
    }
    
    @Override
    public void process(Map map, Tile tile) {
	if (tile.mine())
	    count++;
    }    
}
