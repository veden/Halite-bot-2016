import java.util.ArrayList;
import java.util.TreeSet;

public class APC {
    
    public Site target;
    public final Direction targetDirection;
    
    public ArrayList<UnitDirectionPair> reinforcements;
    
    public APC(Site center) {
	target = center;

	if (center.aboveCombatThreshold()) {
	    MyBot.map.processCardinal(center.x,
				      center.y,
				      site -> {
					  byte owner = MyBot.ai.getOwner(site.x, site.y);
					  if (owner == 0) {
					      int unitTotal = (center.units + site.incoming - site.units);
					      if ((target.defense < site.defense) &&
						  (target.damage < site.damage) &&
						  (center.units > site.units) &&
						  (unitTotal < Constants.SITE_MAX_LOSSY_STRENGTH))
						  target = site;
					  }
				      });

	    if (target == center)
		MyBot.map.processCardinal(center.x,
					  center.y,
					  site -> {
					      byte owner = MyBot.ai.getOwner(site.x, site.y);
					      if (owner == MyBot.ID) {
						  int unitTotal = (center.units + site.incoming + site.units - site.outgoing);
						  if ((target.defense < site.defense) && (unitTotal < Constants.SITE_MAX_LOSSY_STRENGTH))
						      target = site;
					      }
					  });

	    if (target == center)
		MyBot.map.processCardinal(center.x,
					  center.y,
					  site -> {
					      byte owner = MyBot.ai.getOwner(site.x, site.y);
					      if (owner == 0) {
						  if ((target.explore < site.explore) &&
						      MyBot.ai.isObjective(site.x, site.y) &&
						      (center.units > site.units))
						      target = site;
					      }
					  });

	}
	
	if (center.aboveActionThreshold()) {		
	    if (target == center) {
		MyBot.map.processCardinal(center.x,
					  center.y,
					  site -> {
					      int unitTotal = (center.units + site.incoming + site.units - site.outgoing); 
					      if (MyBot.ai.isMine(site.x, site.y) &&
						  (target.strength < site.strength) &&
						  (unitTotal < Constants.SITE_MAX_LOSSY_STRENGTH))
						  target = site;
					  });
	    }

	    if (target == center)
		MyBot.map.processCardinal(center.x,
					  center.y,
					  site -> {
					      if (MyBot.ai.isNeutral(site.x, site.y)) {
						  int unitTotal = (center.units + site.incoming - site.units);
						  if ((target.explore < site.explore) &&
						      (center.units > site.units) &&
						      (unitTotal < Constants.SITE_MAX_LOSSY_STRENGTH))
						      target = site;
					      }
					  });
	}

	target.incoming += center.units;
	center.outgoing += center.units;
	targetDirection = MyBot.map.directionFromSiteToSite(center, target);
    }
}
