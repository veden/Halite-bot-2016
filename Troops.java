import java.util.ArrayList;

public class Troops {
    
    public ArrayList<Move> makeMoves() {
	ArrayList<Move> moves = new ArrayList<Move>();

	for (int i = MyBot.ai.battles.nextSetBit(0); i != -1; i = MyBot.ai.battles.nextSetBit(i+1))
	    takeOpenGround(MyBot.map.getSite(i), moves);
	
	for (Site s : MyBot.map.getFrontSites())
	    pullToPoint(s, moves);
	
	for (Site s: MyBot.map.getOwnedSites())
	    movesFromPoint(s, moves);
	return moves;
    }

    private void takeOpenGround(Site site, ArrayList<Move> moves) {
	final boolean[] enemyOrMine = new boolean[2];
	final short[] strengths = new short[1];
	final Site[] ally = new Site[1];
	MyBot.map.processCardinal(site.x, site.y,
				  s -> {
				      byte owner = MyBot.ai.getOwner(s.x, s.y);
				      if ((owner != 0) && (s.units == 0) && (strengths[0] < s.generator)) {
					  enemyOrMine[0] = true;
					  strengths[0] = s.generator;
				      }
				  });
	MyBot.map.processCardinal(site.x, site.y,
				  s -> {
				      byte owner = MyBot.ai.getOwner(s.x, s.y);
				      if ((owner == MyBot.ID) &&
					  (s.units < 20) &&
					  (enemyOrMine[0] && (s.units > strengths[0])) &&
					  ((ally[0] == null) || ((ally[0] != null) && (ally[0].units > s.units) ))) {
					  enemyOrMine[1] = true;
					  ally[0] = s;
				      }
				  });
	
	if (!enemyOrMine[0] && enemyOrMine[1])
	    moves.add(siteToMove(ally[0], MyBot.map.directionFromSiteToSite(ally[0], site)));
    }
	
    private boolean pullToPoint(Site site, ArrayList<Move> moves) {
	Probe probe = new Probe(site);
	for (Move m : probe.moves)
	    moves.add(m);
	return probe.moved;
    }

    private void movesFromPoint(Site site, ArrayList<Move> moves) {
	if (!MyBot.ai.isUsed(site.x, site.y)) {
	    APC apc = new APC(site);
	    if (apc.target != null)
		moves.add(siteToMove(site, apc.targetDirection));
	}	
    }
    
    public static boolean validUnitDirectionPairs(boolean checkStrength, UnitDirectionPair... ups) {
	for (int i = 0; i < ups.length; i++) {
	    UnitDirectionPair up = ups[i]; 
	    if (!(up != null && !MyBot.ai.isUsed(up.site.x, up.site.y) && ((checkStrength && up.site.aboveActionThreshold()) || !checkStrength)))
		return false;
	}
	return true;
    }

    public static int sumSiteAndReinforcementStrength(Site s, UnitDirectionPair... ups) {
	int value = 0;
	for (int i = 0; i < ups.length; i++)
	    value += ups[i].site.units;
	if (s != null)
	    return value + s.units;
	else
	    return value;
    }

    public static Move unitDirectionToMove(UnitDirectionPair up) {
	MyBot.ai.setUsed(up.site.x, up.site.y);
	return new Move(up.site.x, up.site.y, up.direction);
    }

    public static Move siteToMove(Site s, Direction d) {
	MyBot.ai.setUsed(s.x, s.y);
	return new Move(s.x, s.y, d);
    }
}
