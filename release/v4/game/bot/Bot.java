
package game.bot;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import game.Direction;
import game.GameMap;
import game.Harness;
import game.Site;
import game.bot.model.Entity;

public class Bot extends Entity {
    
    public Bot(byte id, GameMap map) {
	super(id, map);
    }
    
    public void analyze() {
	for (Site s : map.unexplored)
	    if ((s.units != 0) && (s.generator > 0))
		s.explore = SiteUtils.scoreExplore(s, map);
    }

    public void postAnalyze() {

	Collections.sort(frontier,
			 new Comparator<Site>() {
			     @Override
			     public int compare(Site o1, Site o2) {
				 float v = o2.explore - o1.explore;
				 if (v == 0)
				     return o1.id - o2.id;
				 return v > 0 ? 1 : -1;
			     }
			 });
	

	float totalFrontierExplore = 0;
	for (Site s : frontier)
	    totalFrontierExplore += s.explore;
	
	totalFrontierExplore *= Harness.map.frontierRange + (0.30f * (float)totalSites / (float)map.totalSites);
	for (Site s : frontier) {
	    if (totalFrontierExplore > 0) {
		totalFrontierExplore -= s.explore;
		s.set(Site.State.EXPLORE_CANDIDATE);
		for (Site neighbor : s.neighbors.values())
		    if (neighbor.get(Site.State.MINE)) {
			float v = s.explore;
			if (v > neighbor.strength)
			    neighbor.strength = v;
		    }
	    } else
		break;
	}

	Collections.sort(border,
			 new Comparator<Site>() {
			     @Override
			     public int compare(Site o1, Site o2) {
				 float v = o2.strength - o1.strength;
				 if (v == 0)
				     return o1.id - o2.id;
				 return v > 0 ? 1 : -1;
			     }
			 });

	ArrayList<Site> spearHeads = new ArrayList<Site>();
	for (Site s : border)
	    if (s.strength > 0)
		spearHeads.add(s);
	    else
		break;

	for (Site s : spearHeads)
	    SiteUtils.floodFillStrength(s, map);

	for (Site s : battles) {
	    float highestDefense = 0;
	    for (Site neighbor : s.neighbors.values()) {
		if (neighbor.get(Site.State.NEUTRAL) && neighbor.get(Site.State.BATTLE) && (neighbor.defense > highestDefense))
		    highestDefense = neighbor.defense;
	    }
	    if (highestDefense > 0) {
		s.defense = 0.95f * highestDefense;
		SiteUtils.floodFillDefense(s, map, true);
	    }
	}	
    }
    
    public void move() {
	Collections.sort(map.battles,
			 new Comparator<Site>() {
			     @Override
			     public int compare(Site o1, Site o2) {  
				 // float v = o2.defense - o1.defense;
				 // if (v == 0) {
				     float v = o2.damage - o1.damage;
				     if (v == 0)
					 return o1.id - o2.id;
				     //}
				 return v > 0 ? 1 : -1;
			     }
			 });
	for (Site s : map.battles) {
	    if (s.get(Site.State.NEUTRAL) && !s.get(Site.State.FIELD) && MoveUtils.validJoint(s, false))
		for (Direction d : Direction.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);
		    if (neighbor.get(Site.State.MINE)) {
			neighbor.heading = Direction.reverse(d);
			MoveUtils.moveSiteToSite(neighbor, s);
		    }
		}
	}
	for (Site s : battles) {
	    for (Direction d : Direction.CARDINALS) {
		Site neighbor = s.neighbors.get(d);
		if (MoveUtils.validAttack(s, neighbor) &&
		    (s.defense < neighbor.defense) &&
		    ((neighbor.damage > s.target().damage) ||
		     ((neighbor.damage == 0) && (neighbor.defense > s.target().defense))))
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
		if (MoveUtils.validCapture(neighbor, s) && ((lowest == null) || (lowest.defense > neighbor.defense))) {
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
	    if (s.get(Site.State.EXPLORE_CANDIDATE)) {
		if (MoveUtils.validJoint(s, true))
		    for (Direction d : Direction.CARDINALS) {
			Site neighbor = s.neighbors.get(d);	
			if (neighbor.get(Site.State.MINE)) {
			    neighbor.heading = Direction.reverse(d);
			    MoveUtils.moveSiteToSite(neighbor, s);
			}
		    }
	    }
	}
	for (Site s : border) {
	    for (Direction d : Direction.CARDINALS) {
		Site neighbor = s.neighbors.get(d);
		if (MoveUtils.validExplore(s, neighbor) && (s.target().explore < neighbor.explore))
		    s.heading = d;
	    }

	    if ((s.heading == Direction.STILL) && s.get(Site.State.READY))
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

	    MoveUtils.moveSiteToSite(s, s.target());
	}

	Collections.sort(interior,
			 new Comparator<Site>() {
			     @Override
			     public int compare(Site o1, Site o2) {
				 float v = o1.units - o2.units;
				 if (v == 0)
				     return o1.id - o2.id;
				 return v > 0 ? 1 : -1;
			     }
			     
			 });
	
	for (Site s : interior) {
	    if (s.get(Site.State.READY)) {
		for (Direction d : Direction.CARDINALS) {
		    Site neighbor = s.neighbors.get(d);
		    if (MoveUtils.validMove(s, neighbor) && (s.target().defense < neighbor.defense))
			s.heading = d;
		}

		if (s.heading == Direction.STILL)
		    for (Direction d : Direction.CARDINALS) {
			Site neighbor = s.neighbors.get(d);
			if (MoveUtils.validMove(s, neighbor) && (s.target().strength < neighbor.strength))
			    s.heading = d;
		    }

		MoveUtils.moveSiteToSite(s, s.target());
	    }
	}
    }
}
