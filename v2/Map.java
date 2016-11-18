import java.util.ArrayList;
import java.util.HashMap;

public class Map {

    private HashMap<Integer, HashMap<Integer, Tile>> tiles;

    public final int width;
    public final int height; 

    private AnalysisProcess analysisProcess;
    private DispersalProcess dispersalProcess;
    
    public Map(GameMap gameMap) {
	width = gameMap.width;
	height = gameMap.height;
	tiles = new HashMap<Integer, HashMap<Integer, Tile>>();
	analysisProcess = new AnalysisProcess();
	
	//assuming square map
	dispersalProcess = new DispersalProcess((int)Math.floor(width / 2.0f) - 1);
	refresh(gameMap);
    }

    public ArrayList<Tile> getOwnedTiles() {
	return analysisProcess.rankedOwnedTiles;
    }

    public ArrayList<Tile> getFrontTiles() {
	return analysisProcess.rankedFrontTiles;
    }

    public float manhattanDistance(Tile t1, Tile t2) {
	int dx = Math.abs(t1.x - t2.x);
	int dy = Math.abs(t1.y - t2.y);

	if(dx > width * 0.5) dx = width - dx;
	if(dy > height * 0.5) dy = height - dy;

	return dx + dy;
    }
    
    public void refresh(GameMap gameMap) {
	Location l = new Location(0, 0);
	for (int x = 0; x < gameMap.width; x++)
	    for (int y = 0; y < gameMap.height; y++) {
		l.x = x;
		l.y = y; 
		updateTile(x, y, gameMap.getSite(l));
	    }

	processRegion(0, 0, width, height, dispersalProcess);
	
	analysisProcess.reset();
	
	processRegion(0, 0, width, height, analysisProcess);

	analysisProcess.postProcess();

	MyBot.printLn(Tile.getGeneratorScaling() + "-gs, " + Tile.getGeneratorMin() + "-gmi, " + Tile.getGeneratorMax() + "-gma, " + Tile.getGeneratorAverage() + "-ga, " +
		      Tile.getUnitMin() + "-umi, " + Tile.getUnitMax() + "-uma, " + Tile.getUnitAverage() + "-ua");
	    
	ArrayList<Tile> rankedOwnedTiles = getOwnedTiles();
	MyBot.printLn(rankedOwnedTiles.size()+"");
	for (int i = 0; i < rankedOwnedTiles.size(); i++)
	    MyBot.printLn(rankedOwnedTiles.get(i).toString());
	MyBot.printLn("--");
	ArrayList<Tile> rankedFrontTiles = getFrontTiles();
	float totalProduction = 0.0f;
	MyBot.printLn(rankedFrontTiles.size()+"");
	for (int i = 0; i < rankedFrontTiles.size(); i++) {
	    Tile t = rankedFrontTiles.get(i);
	    totalProduction += t.production;
	    MyBot.printLn(t.toString());
	}
	MyBot.printLn("p-" + (totalProduction / rankedFrontTiles.size()));
	MyBot.printLn("");

	// for (int x = 0; x < gameMap.width; x++)
	//     for (int y = 0; y < gameMap.height; y++) {
	// 	MyBot.printLn(getTile(x, y).toString());
	//     }
    }
    
    public Tile getTile(int x, int y) {
	return tiles.get(safeCoordinate(x, width)).get(safeCoordinate(y, height));
    }
    
    public Tile updateTile(int x, int y, Site site) {
	HashMap<Integer, Tile> yMap = tiles.get(x);
	if (yMap == null) {
	    yMap = new HashMap<Integer, Tile>();
	    tiles.put(x, yMap);
	}

	Tile t = yMap.get(y);
	if (t != null)
	    t.updateTile(site);
	else {
	    t = new Tile(x, y, site);
	    yMap.put(y, t);
	}
	return t;
    }

    public void processCardinal(int sX, int sY, ProcessTile pu) {
	pu.process(this, getTile(sX, sY-1)); //north
	pu.process(this, getTile(sX+1, sY)); //east
	pu.process(this, getTile(sX, sY+1)); //south
	pu.process(this, getTile(sX-1, sY)); //west
    }

    public void processRegion(int sX, int sY, int eX, int eY, ProcessTile pu) {
	for (int x = sX; x < eX; x++)
	    for (int y = sY; y < eY; y++)
		pu.process(this, getTile(x, y));
    }
    
    private static int safeCoordinate(int x, int limit) {
	if (x < 0) 
	    return limit + (x % limit);
	else
	    return x % limit;
    }

    //does only one cardinal adjacent tile
    public Direction directionFromTileToTile(Tile a, Tile b) {	
	if (a.equals(b))
	    return Direction.STILL;
	if ((safeCoordinate(a.x + 1, width) == b.x) && (a.y == b.y))
	    return Direction.EAST;
	else if ((safeCoordinate(a.x - 1, width) == b.x) && (a.y == b.y))
	    return Direction.WEST;
	else if ((safeCoordinate(a.y + 1, height) == b.y) && (a.x == b.x))
	    return Direction.SOUTH;
	else if ((safeCoordinate(a.y - 1, height) == b.y) && (a.x == b.x))
	    return Direction.NORTH;
	return null;
    }

    public static Direction toDirection(int i) {
	if (i == 0)
	    return Direction.STILL;
	else if (i == 1)
	    return Direction.NORTH;
	else if (i == 2)
	    return Direction.EAST;
	else if (i == 3)
	    return Direction.SOUTH;
	else if (i == 4)
	    return Direction.WEST;
	return null;
    }
    
    public static Direction toInverseDirection(int i) {
	if (i == 0)
	    return Direction.STILL;
	else if (i == 1)
	    return Direction.SOUTH;
	else if (i == 2)
	    return Direction.WEST;
	else if (i == 3)
	    return Direction.NORTH;
	else if (i == 4)
	    return Direction.EAST;
	return null;
    }
}
