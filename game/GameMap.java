package game;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.function.Predicate;

import game.Site.State;

import logic.AI;
import logic.model.Enemy;
import logic.model.Entity;
import logic.util.RingIterator;

public class GameMap{
    public static final float MAX_SIZE = 50f;

    public Site[] sites;
    public int width;
    public int height;
    public float scaler;

    public AI bot;

    public HashMap<Integer, Enemy> enemies = new HashMap<Integer, Enemy>();
    
    public ArrayList<Site> unexplored = new ArrayList<Site>();
    
    public void reset() {
	unexplored.clear();
	Stats.reset();
	bot.reset();
	for (Entry<Integer, Enemy> e : enemies.entrySet())
	    e.getValue().reset();
    }

    public void buildSites(int width, int height) {
        this.width = width;
        this.height = height;
	this.scaler = (Math.min(width, height) - 1) * 0.5f;
	Stats.totalSites = width * height;
	sites = new Site[width * height];
	for(int x = 0; x < width; x++)
            for(int y = 0; y < height; y++)
		sites[y + (height * x)] = new Site(x, y, height);
    }

    public void addUnexplored(Site s) {
	unexplored.add(s);
        Stats.totalUnexploredGenerator += s.generator;
	s.set(State.UNEXPLORED);
    }
    
    public float manhattanDistance(Site s1, Site s2) {
        int dx = Math.abs(s1.x - s2.x);
        int dy = Math.abs(s1.y - s2.y);

        if(dx > width / 2.0) dx = width - dx;
        if(dy > height / 2.0) dy = height - dy;

        return dx + dy;
    }
    
    public Site getSite(int x, int y) {
	return sites[safeCoordinate(y, height) + (height * safeCoordinate(x, width))];
    }

    public void collectStats() {
	for (int i = 0; i < sites.length; i++) {
	    Site s = sites[i];
	    if (s.explore > Stats.maxExplore)
		Stats.maxExplore = s.explore;
	    if (s.explore < Stats.minExplore)
		Stats.minExplore = s.explore;
	    if (s.reinforce > Stats.maxReinforce)
		Stats.maxReinforce = s.reinforce;
	    if (s.reinforce < Stats.minReinforce)
		Stats.minReinforce = s.reinforce;
	    if (s.damage > Stats.maxDamage)
		Stats.maxDamage = s.damage;
	    if (s.damage < Stats.minDamage)
		Stats.minDamage = s.damage;
	}
    }
    
    public void classifySite(Site site) {
	if (site.get(State.MINE) || site.get(State.ENEMY)) {
	    if (site.units == 0)
		site.set(State.USED);
	    if (site.aboveActionThreshold())
		site.set(State.READY);
	    if (site.aboveCombatThreshold())
		site.set(State.COMBAT_READY);
	    
	    Entity e;
	    if (site.get(State.MINE))
		e = bot;
	    else 
	        e = getEnemy(site.owner);
	    
	    boolean border = false;
	    for (Site neighbor : site.neighbors.values())
		if (neighbor.get(State.NEUTRAL)) {
		    border = true;
		    break;
		} 
	    
	    if (!border)
	        e.addInterior(site);
	    else
	        e.addBorder(site);
	} else if (site.get(State.NEUTRAL)) {
	    if (site.units == 0) {
		HashMap<Integer, Boolean> neighborCheck = new HashMap<Integer, Boolean>();
		for (Site neighbor : site.neighbors.values())
		    if (neighbor.get(State.MINE)) {
			bot.addBattle(neighbor);
			neighborCheck.put(bot.id, true);
		    } else if (neighbor.get(State.ENEMY)) {
			getEnemy(neighbor.owner).addBattle(neighbor);
			neighborCheck.put(neighbor.owner, true);
		    }
		if (neighborCheck.size() == 1) {
		    for (Site neighbor : site.neighbors.values())
			if (neighbor.get(State.MINE))
			    bot.addOpen(site);
			else if (neighbor.get(State.ENEMY)) 
			    getEnemy(neighbor.owner).addOpen(site);
		    site.set(State.OPEN);
		}
		site.set(State.BATTLE);
	    } else {
		boolean frontier = false;
		boolean frontierEnemy = false;
		for (Site neighbor : site.neighbors.values()) {
		    if (neighbor.get(State.MINE))
			frontier = true;
		    else if (neighbor.get(State.ENEMY))
			frontierEnemy = true;
		}
		if (frontier && frontierEnemy) {
		    for (Site neighbor : site.neighbors.values())
			if (neighbor.get(State.MINE))
			    bot.addGate(neighbor);
			else if (neighbor.get(State.ENEMY))
			    getEnemy(neighbor.owner).addGate(neighbor);
		    site.set(State.GATE);		
		} else {
		    for (Site neighbor : site.neighbors.values())
			if (neighbor.get(State.MINE))
			    bot.addFrontier(site);
			else if (neighbor.get(State.ENEMY))
			    getEnemy(neighbor.owner).addFrontier(site);
		    addUnexplored(site);
		}
	    }
	}
    }

    public float normalize(float x, float low, float high) {
    	return ((x - low) / (high - low));
    }
    
    public void scoreUnexplored() {
	if (unexplored.size() == 0)
	    return;
	
	Predicate<Site> p = new Predicate<Site>() {
		@Override
		public boolean test(Site t) {
		    return t.get(State.UNEXPLORED) && (t.explore != 0);
		}
	    };

	Collections.sort(unexplored,
			 new Comparator<Site>() {
			     @Override
			     public int compare(Site arg0, Site arg1) {
				 float v = arg1.getExploreValue() - arg0.getExploreValue();
				 if (v == 0)
				     return arg0.id - arg1.id;
				 return v > 0 ? 1 : -1;
			     }
			 });

	ArrayList<Site> objectivePoints = new ArrayList<Site>();

	float minExplore = unexplored.get(unexplored.size() - 1).getExploreValue();
	float maxExplore = unexplored.get(0).getExploreValue();
	
	for (Site s : unexplored) {
	    if (normalize(s.getExploreValue(), minExplore, maxExplore) > 0.60) {
		if (s.explore < s.getExploreValue())
		    s.explore = s.getExploreValue();
		for (Site n : s.neighbors.values())
		    if (n.get(State.UNEXPLORED) && (s.explore >= n.explore))
			n.explore = s.explore * 0.98f;
		objectivePoints.add(s);
	    }
	}

	RingIterator riop = new RingIterator(objectivePoints, p);

	while (riop.hasNext()) {
	    boolean changed = false;
	    ArrayList<Site> sortedNeighbors = riop.next();
	    Collections.sort(sortedNeighbors,
	    		     new Comparator<Site>() {
	    			 @Override
	    			 public int compare(Site arg0, Site arg1) {
	    			     float v = arg1.explore - arg0.explore;
				     if (v == 0)
	    				 return arg0.id - arg1.id;
	    			     return v > 0 ? 1 : -1;
	    			 }
	    		     });
	    for (Site s : sortedNeighbors) {
		float absorb = s.explore * (0.98f - (0.45f * (s.units / Site.MAX_STRENGTH)) - (0.125f * (1 - (s.generator / Stats.maxGenerator))));
		for (Site sn : s.neighbors.values()) {
		    if (sn.get(State.UNEXPLORED) && (absorb > sn.explore)) {
			sn.explore = absorb;
			changed = true;
		    }
		}
	    }
	    if (!changed)
	    	break;
	}
    }

    public int safeCoordinate(int x, int limit) {
	if (x < 0) 
	    return limit + (x % limit);
	else
	    return x % limit;
    }

    public void identifyEnemy() {
	for (Entry<Integer, Enemy> e : enemies.entrySet()) {
	    Enemy enemy = e.getValue();
	    enemy.placeDefense();
	}
    }
    
    public Enemy getEnemy(int id) {
	if (!enemies.containsKey(id))
	    enemies.put(id, new Enemy(id, this));
	return enemies.get(id);
    }

    public String enemiesToReplay() {
	StringBuilder sb = new StringBuilder();
	for (Entry<Integer, Enemy> e : enemies.entrySet()) {
	    if (sb.length() > 0)
		sb.append("\n");
	    sb.append(e.getValue().toString());
	}
	return sb.toString();
    }
}
