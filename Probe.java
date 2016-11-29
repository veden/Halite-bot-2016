import java.util.ArrayList;

public class Probe {
    public final ArrayList<Move> moves;

    public boolean moved = false;
    
    public Probe(Site target) {
	moves = new ArrayList<Move>();

	final ArrayList<UnitDirectionPair> layer1 = new ArrayList<UnitDirectionPair>();
	
	ProcessSite getNeighbors = new ProcessSite() {
		@Override
		public void process(Site site) {
		    if (site.mine())
			layer1.add(new UnitDirectionPair(site, MyBot.map.directionFromSiteToSite(site, target)));
		}
	    };
	
	MyBot.map.processCardinal(target.x, target.y, getNeighbors);

	for (int i = 0; i < layer1.size() && !moved; i++) {
	    UnitDirectionPair upI = layer1.get(i);
	    if (Troops.validUnitDirectionPairs(true, upI) && (upI.site.units > target.units)) {
		upI.site.outgoing += upI.site.units;
		target.incoming += upI.site.units;
		moves.add(Troops.unitDirectionToMove(upI));
		moved = true;
	    }
	}

	for (int i = 0; i < layer1.size() && !moved; i++) {
	    UnitDirectionPair upI = layer1.get(i);
	    for (int x = i+1; x < layer1.size() && !moved; x++) {
		UnitDirectionPair upX = layer1.get(x);
		if (Troops.validUnitDirectionPairs(true, upI, upX) && (Troops.sumSiteAndReinforcementStrength(null, upI, upX) > target.units)) {
		    upI.site.outgoing += upI.site.units;
		    target.incoming += upI.site.units;
		    upX.site.outgoing += upX.site.units;
		    target.incoming += upX.site.units;
		    moves.add(Troops.unitDirectionToMove(upI));
		    moves.add(Troops.unitDirectionToMove(upX));
		    moved = true;
		}
	    }
	}

	for (int i = 0; i < layer1.size() && !moved; i++) {
	    UnitDirectionPair upI = layer1.get(i);
	    for (int x = i+1; x < layer1.size() && !moved; x++) {
		UnitDirectionPair upX = layer1.get(x);
		for (int y = x+1; y < layer1.size() && !moved; y++) {
		    UnitDirectionPair upY = layer1.get(y);
		    if (Troops.validUnitDirectionPairs(true, upI, upX, upY) && (Troops.sumSiteAndReinforcementStrength(null, upI, upX, upY) > target.units)) {
			upI.site.outgoing += upI.site.units;
			target.incoming += upI.site.units;
			upX.site.outgoing += upX.site.units;
			target.incoming += upX.site.units;
			upY.site.outgoing += upY.site.units;
			target.incoming += upY.site.units;
			moves.add(Troops.unitDirectionToMove(upI));
			moves.add(Troops.unitDirectionToMove(upX));
			moves.add(Troops.unitDirectionToMove(upY));
			moved = true;
		    }
		}
	    }
	}
    }
}
