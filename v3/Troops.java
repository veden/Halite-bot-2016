import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.TreeMap;

public class Troops {
    
    public static float lostStrength = 0.0f;
    
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

	optimizeMoves(map, moves);
	return moves;
    }

    public void optimizeMoves(Map map, ArrayList<Move> moves) {
	HashMap<Tile, ArrayList<Move>> used = new HashMap<Tile, ArrayList<Move>>();
	TreeMap<Tile, Move> herd = new TreeMap<Tile, Move>(new Comparator<Tile>() {
		@Override
		public int compare(Tile arg0, Tile arg1) {
		    float v = arg0.defense - arg1.defense;
		    if (v == 0) {
			v = arg1.strength - arg0.strength;
			if (v == 0)
			    return arg0.id - arg1.id;
		    }
		    return v>0?1:-1;
		}
	    });
	for (int x = 0; x < moves.size(); x++) {
	    Move m = moves.get(x);
	    Tile dt = map.getTileWithPositionDirection(m.loc.x, m.loc.y, m.dir);
	    Tile t = map.getTile(m.loc.x, m.loc.y);
	    if (!used.containsKey(dt)) 
		used.put(dt, new ArrayList<Move>());
	    used.get(dt).add(m);
	    if ((t.defense > 0) && (t.defense < 0.3))
		herd.put(t, m);
	}
	
	//prevent strength leak
	for (Tile t : used.keySet()) {
	    ArrayList<Move> tileMoves = used.get(t);
	    float total = 0.0f;
	    if (t.mine())
		total += t.getUnits();
	    else
		total -= t.getUnits();
	    for (int i = 0; i < tileMoves.size(); i++) {
		Move m = tileMoves.get(i);
		Tile t1 = map.getTile(m.loc.x, m.loc.y);
		if (total + t1.getUnits() >= Constants.SITE_MAX_STRENGTH + 15) {
		    m.dir = Direction.STILL;
		    
		} else
		    total += t1.getUnits();
	    }
	    if (total > Constants.SITE_MAX_STRENGTH)
		lostStrength += total - Constants.SITE_MAX_STRENGTH;
	}
	
	//compressing strength
	for (Tile t1 : herd.keySet()) {
	    Move m1 = herd.get(t1);
	    map.processRegion(t1.x - 2,
			      t1.y - 2,
			      t1.x + 2,
			      t1.y + 2,
			      new ProcessTile() {
				  @Override
				  public void process(Map map, Tile t2) {
				      if (!t2.equals(t1) && !t1.optimized && !t2.optimized) {
					  Move m2 = herd.get(t2);
					  if ((m2 != null) && (m2.dir == m1.dir)) {
					      float distance = map.manhattanDistance(t1, t2);
					      if (distance <= 2) {
						  if (distance == 2) {
						      // MergeProcess mp = new MergeProcess(t1, t2);
						      // map.processCardinal(t1.x,
						      // 			  t1.y,
						      // 			  mp);
						      // Tile target = mp.getTarget();
						      // if (target != null) {
						      // 	  t1.optimized = true;
						      // 	  t2.optimized = true;
						      // 	  Direction d1 = map.directionFromTileToTile(t1, target);
						      // 	  Direction d2 = map.directionFromTileToTile(t2, target);
						      // 	  m1.dir = d1;
						      // 	  m2.dir = d2;
						      // 	  MyBot.printLn("waBam1 " + d1.ordinal() + ", " + d2.ordinal());
						      // 	  MyBot.printLn(t1.toString());
						      // 	  MyBot.printLn(t2.toString());
						      // 	  MyBot.printLn(target.toString());
						      // }
						  } else {
						      if (t2.getUnits() + t1.getUnits() < Constants.SITE_MAX_STRENGTH + 15) {
						      	  t1.optimized = true;
						      	  t2.optimized = true;
						      	  if (t1.strength > t2.strength) {
						      	      m2.dir = map.directionFromTileToTile(t2, t1);
						      	      m1.dir = Direction.STILL;
						      	  } else {
						      	      m2.dir = Direction.STILL;
						      	      m1.dir = map.directionFromTileToTile(t1, t2); 
						      	  }
							      
						      	  MyBot.printLn("waBam2");
						      	  MyBot.printLn(t1.toString());
						      	  MyBot.printLn(t2.toString());
						      }
						  }
					      }
					  }
				      }
				  }
			      });
	}
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
