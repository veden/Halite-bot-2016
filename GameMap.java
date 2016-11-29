import java.util.TreeSet;

public class GameMap{
    private Site[] sites;
    public int width;
    public int height;

    private AnalysisProcess analysisProcess;
    private LayerProcess layerProcess;
    
    public int dispersalReach;
    
    public GameMap(int width_, int height_) {
        width = width_;
        height = height_;
        sites = new Site[width * height];
        for(byte x = 0; x < width; x++)
            for(byte y = 0; y < height; y++)
		sites[y + (height * x)] = new Site(x, y);
	
	this.dispersalReach = (int)Math.floor(Math.min(width, height) / 3.5f) - 1;
	layerProcess = new LayerProcess(dispersalReach);

	analysisProcess = new AnalysisProcess(dispersalReach);
    }
    
    public float manhattanDistance(Site s1, Site s2) {
        int dx = Math.abs(s1.x - s2.x);
        int dy = Math.abs(s1.y - s2.y);

        if(dx > width / 2.0) dx = width - dx;
        if(dy > height / 2.0) dy = height - dy;

        return dx + dy;
    }

    public Site getSite(int x, int y) {
	return sites[Utils.safeCoordinate(y, height) + (height * Utils.safeCoordinate(x, width))];
    }

    public Site getSite(int i) {
	return sites[i];
    }

    public Ring getRing(Site s) {
	return layerProcess.siteToRings.get(s);
    }

    public float getMaxExplore() {
	return analysisProcess.maxExplore;
    }
    
    public TreeSet<Site> getOwnedSites() {
	return analysisProcess.rankedOwnedSites;
    }

    public TreeSet<Site> getFrontSites() {
	return analysisProcess.rankedFrontSites;
    }
    
    public void refresh() {
        processAll(layerProcess);

	analysisProcess.reset();
	
	processAll(analysisProcess);

	analysisProcess.postProcess();
	
	//	MyBot.printLn(width + "-w " + height + "-h " + analysisProcess.maxExplore + "-pt " + Troops.lostStrength + "-ls");
	
        // TreeSet<Site> rankedOwnedSites = getOwnedSites();
	// MyBot.printLn(rankedOwnedSites.size()+"");
	// for (Site t : rankedOwnedSites)
	//     MyBot.printLn(t.toString());
	// MyBot.printLn("--");
	// TreeSet<Site> rankedFrontSites = getFrontSites();
	// MyBot.printLn(rankedFrontSites.size()+"");
	// for (Site t : rankedFrontSites)
	//     MyBot.printLn(t.toString());
	// MyBot.printLn("");

	// for (int x = 0; x < width; x++)
	//     for (int y = 0; y < height; y++) {
	// 	MyBot.printReplay(getSite(x, y).encodeString());
	//     }
    }

    public void processCardinal(int sX, int sY, ProcessSite pu) {
	pu.process(getSite(sX, sY-1)); //north
	pu.process(getSite(sX+1, sY)); //east
	pu.process(getSite(sX, sY+1)); //south
	pu.process(getSite(sX-1, sY)); //west
    }

    public void processAll(ProcessSite pu) {
	for (int i = 0; i < sites.length; i++)
	    pu.process(sites[i]);
    }

    public void processRegion(int sX, int sY, int eX, int eY, ProcessSite pu) {
	for (int x = sX; x < eX; x++)
	    for (int y = sY; y < eY; y++)
		pu.process(getSite(x, y));
    }

    //does only one cardinal adjacent tile
    public Direction directionFromSiteToSite(Site a, Site b) {	
	if (a.equals(b))
	    return Direction.STILL;
	if ((Utils.safeCoordinate(a.x + 1, width) == b.x) && (a.y == b.y))
	    return Direction.EAST;
	else if ((Utils.safeCoordinate(a.x - 1, width) == b.x) && (a.y == b.y))
	    return Direction.WEST;
	else if ((Utils.safeCoordinate(a.y + 1, height) == b.y) && (a.x == b.x))
	    return Direction.SOUTH;
	else if ((Utils.safeCoordinate(a.y - 1, height) == b.y) && (a.x == b.x))
	    return Direction.NORTH;
	return null;
    }

    public double getAngle(Site s1, Site s2) {
        int dx = s1.x - s2.x;

        // Flip order because 0,0 is top left
        // and want atan2 to look as it would on the unit circle
        int dy = s2.y - s1.y;

        if(dx > width - dx) dx -= width;
        if(-dx > width + dx) dx += width;

        if(dy > height - dy) dy -= height;
        if(-dy > height + dy) dy += height;

        return Math.atan2(dy, dx);
    }
    
    public Site getSiteWithPositionDirection(int x, int y, Direction d) {
	if (d == Direction.NORTH)
	    return getSite(x, y-1);
	else if (d == Direction.SOUTH)
	    return getSite(x, y+1);
	else if (d == Direction.EAST)
	    return getSite(x+1, y);
	else if (d == Direction.WEST)
	    return getSite(x-1, y);
	else if (d == Direction.STILL)
	    return getSite(x, y);
	return null;
    }
}
