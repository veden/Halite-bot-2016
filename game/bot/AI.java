
package game.bot;

import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;

import game.Direction;
import game.GameMap;
import game.Site;
import game.Stats;
import game.bot.model.Entity;
import game.bot.util.FloodFunction;
import game.bot.util.MoveUtils;
import game.bot.util.SiteFunction;
import game.bot.util.SiteUtils;

public class AI extends Entity {

    public AI(byte id, GameMap map) {
	super(id, map);
    }
    
    public SiteFunction scoreExplore = new SiteFunction() {

	    @Override
	    public void process(Site s, BitSet used, HashSet<Site> current, HashSet<Site> next, Site center, float distance) {
		if (s.get(Site.State.NEUTRAL) && (s.units != 0)) {
		    totalWeight += (1 - (0.30f * distance));
		    total += s.getExploreValue();
		    if ((distance+1<3) && (!used.get(s.id))) {
			next.add(s);
			used.set(s.id);
		    }
		}
	    }
	};
    
    public FloodFunction formDefense = new FloodFunction() {
	    
	    @Override
	    public void scan(Site neighbor, Site center) {
		if (best < neighbor.defense)
		    best = neighbor.defense;		
	    }

	    @Override
	    public void process(Site s, BitSet used, Site center, float distance, HashSet<Site> current, HashSet<Site> next) {
		if (distance >= 6)
		    abort = true;
		float v = (1 - (0.03f * distance)) * best;
		if (v > s.defense)
		    s.defense = v;
		for (Site neighbor : s.neighbors.values())
		    if ((neighbor.owner == center.owner) && !used.get(neighbor.id))
			next.add(neighbor);
	    }
	};
    
    public FloodFunction strengthToExplore = new FloodFunction() {

	    @Override
	    public void scan(Site neighbor, Site center) {
		if (best < neighbor.strength)
		    best = neighbor.strength;
	    }
	    
	    @Override
	    public void process(Site s, BitSet used, Site center, float distance, HashSet<Site> current, HashSet<Site> next) {
		float v = (1 - (0.04f * distance)) * best;
		if ((v > s.strength) && (s.defense == 0))
		    s.strength = v;
		if (v > 0)
		    for (Site neighbor : s.neighbors.values())
			if (neighbor.get(Site.State.MINE) && !used.get(neighbor.id))
			    next.add(neighbor);		
	    }
	};
    
    public void analyze() { 
	for (Site s : map.unexplored)
	    if ((s.units != 0) && (s.generator > 0))
		s.explore = SiteUtils.scoreWeighted(s, map, scoreExplore.setInitial(s.getExploreValue()));
    }

    public void postAnalyze() {

	if (frontier.size() < 6)
	    Stats.defenseRange = 0.01f;
	
	for (Site s : battles) {
	    float highestDefense = 0;
	    for (Site neighbor : s.neighbors.values()) {
		if (neighbor.get(Site.State.NEUTRAL) && neighbor.get(Site.State.BATTLE) && (neighbor.defense > highestDefense))
		    highestDefense = neighbor.defense;
	    }
	    if (highestDefense > 0)
	    	SiteUtils.flood(s, map, formDefense.setInitial(0.95f * highestDefense));
	}

	// Collections.sort(frontier,
	// 		 new Comparator<Site>(){
	// 		     @Override
	// 		     public int compare(Site o1, Site o2) {
	// 			 float v = o2.explore - o1.explore;
	// 			 if (v==0)
	// 			     return o1.id - o2.id;
	// 			 return v > 0 ? 1 : -1;
	// 		     }
	// 		 });

	// float totalExplore = 0f;
	// for (Site s : frontier)
	//     totalExplore += s.explore;

	//	totalExplore *= 0.55f;
	
	for (Site s : frontier) {
	    // if (totalExplore > 0) {
	    // 	boolean valid = false;
		for (Site neighbor : s.neighbors.values())
		    if (neighbor.get(Site.State.MINE) && (neighbor.defense == 0)) {
			float v = s.explore;
			//			valid = true;
			if (v > neighbor.strength)
			    neighbor.strength = v;
		    }
		// if (valid)
		//     totalExplore -= s.explore;
	    // } else
	    // 	break;
	}
	
	for (Site s : border)
	    if (s.defense == 0)
		SiteUtils.flood(s, map, strengthToExplore.setInitial(s.strength));
    }
    
    public void move() {
	Collections.sort(map.battles,
			 new Comparator<Site>() {
			     @Override
			     public int compare(Site o1, Site o2) {  
				 float v = o2.defense - o1.defense;
				 if (v == 0) {
				     v = o2.damage - o1.damage;
				     if (v == 0)
					 return o1.id - o2.id;
				 }
				 return v > 0 ? 1 : -1;
			     }
			 });
	for (Site s : map.battles) {
	    if (s.get(Site.State.NEUTRAL) && !s.get(Site.State.FIELD) && MoveUtils.validJoint(s, false))
		for (Direction d : Direction.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);
		    if (neighbor.get(Site.State.MINE) && MoveUtils.validAttack(neighbor, s)) {
			neighbor.heading = Direction.reverse(d);
			MoveUtils.moveSiteToSite(neighbor, s);
		    }
		}
	}
	for (Site s : battles) {
	    for (Direction d : Direction.CARDINALS) {
		Site neighbor = s.neighbors.get(d);
		if (MoveUtils.validAttack(s, neighbor) && (s.defense < neighbor.defense) && (neighbor.damage > s.target().damage))
		    s.heading = d;
	    }

	    if (MoveUtils.moveSiteToSite(s, s.target()))
		SiteUtils.reduceDamage(s);
	}
	for (Site s : map.fields) {
	    Site lowest = null;
	    Direction lowestD = null;
	    for (Direction d : Direction.CARDINALS) {
		Site neighbor = s.neighbors.get(d);
		if (MoveUtils.validCapture(neighbor, s) && ((lowest == null) || (lowest.units > neighbor.units))) {
		    lowestD = Direction.reverse(d);
		    lowest = neighbor;
		}
	    }
	    if (lowest != null) {
		lowest.heading = lowestD;
		MoveUtils.moveSiteToSite(lowest, s);
	    }
	}
	for (Site s : frontier) {
	    if (MoveUtils.validJoint(s, true))
		for (Direction d : Direction.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);	
		    if (neighbor.get(Site.State.MINE) && MoveUtils.validExplore(neighbor, s)) {
			neighbor.heading = Direction.reverse(d);
			MoveUtils.moveSiteToSite(neighbor, s);
		    }
		}
	}
	for (Site s : border) {
	    if (s.get(Site.State.READY))
		for (Direction d : Direction.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);
		    if (MoveUtils.validMove(s, neighbor) && (s.target().defense < neighbor.defense))
			s.heading = d;
		}

	    if ((s.heading == Direction.STILL) && s.get(Site.State.READY))
		for (Direction d : Direction.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);
		    if (MoveUtils.validMove(s, neighbor) && (s.target().strength < neighbor.strength))
			s.heading = d;
		}
	    
	    if (s.heading == Direction.STILL)
		for (Direction d : Direction.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);
		    if (MoveUtils.validExplore(s, neighbor) && (s.target().explore < neighbor.explore))
			s.heading = d;
		}
	    

	    MoveUtils.moveSiteToSite(s, s.target());
	}

	for (Site s : interior) {
	    if (s.get(Site.State.READY)) {
		for (Direction d : Direction.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);
		    if (MoveUtils.validMove(s, neighbor) &&
			(s.defense < neighbor.defense) &&
			((s.heading == Direction.STILL) || (MoveUtils.totalUnits(s, s.target()) < MoveUtils.totalUnits(s, neighbor))))
			s.heading = d;
		}

		if (s.heading == Direction.STILL)
		    for (Direction d : Direction.CARDINALS) {
			Site neighbor = s.neighbors.get(d);
			if (MoveUtils.validMove(s, neighbor) &&
			    (s.strength < neighbor.strength) &&
			    ((s.heading == Direction.STILL) || (((s.units == 255) && (MoveUtils.totalUnits(s, s.target()) > MoveUtils.totalUnits(s, neighbor))) ||
								((s.units != 255) && (MoveUtils.totalUnits(s, s.target()) < MoveUtils.totalUnits(s, neighbor))))))
			    s.heading = d;
		    }

		MoveUtils.moveSiteToSite(s, s.target());
	    }
	}
    }
}
