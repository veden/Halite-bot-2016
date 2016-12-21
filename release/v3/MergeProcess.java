public class MergeProcess implements ProcessTile {

    private Tile t1;
    private Tile t2;

    private Tile target = null;
    
    public MergeProcess(Tile t1, Tile t2) {
	this.t1 = t1;
	this.t2 = t2;
    }

    public Tile getTarget() {
	return target;
    }
    
    @Override
    public void process(Map map, Tile aggro) {
	if (aggro.mine() || (aggro.getUnits() == 0)) {
	    if (aggro.getUnits() + t2.getUnits() + t1.getUnits() < Constants.SITE_MAX_STRENGTH + 15) {
		Direction a = map.directionFromTileToTile(t2, aggro);
		if ((a != null) && (aggro.strength > t1.strength) && (aggro.strength > t2.strength) && ((target == null) || (target.strength < aggro.strength)))
		    target = aggro;
	    }
	}
    }
}
