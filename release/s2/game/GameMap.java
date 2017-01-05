package game;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.function.Predicate;

import game.Site.P;
import game.Site.State;

import logic.AI;
import logic.Parameters;
import logic.model.Enemy;
import logic.model.Entity;
import logic.util.CompareUtil;
import logic.util.RingIterator;

public class GameMap{
    public static final float MAX_SIZE = 50f;

    public Site[] sites;
    public int width;
    public int height;
    public float scaler;
    public float mapScaling;

    public boolean processedExploreValues = false;

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
	    if (s.value(P.EXPLORE) > Stats.maxExplore)
		Stats.maxExplore = s.value(P.EXPLORE);
	    if (s.value(P.EXPLORE) < Stats.minExplore)
		Stats.minExplore = s.value(P.EXPLORE);
	    if (s.value(P.REINFORCE) > Stats.maxReinforce)
		Stats.maxReinforce = s.value(P.REINFORCE);
	    if (s.value(P.REINFORCE) < Stats.minReinforce)
		Stats.minReinforce = s.value(P.REINFORCE);
	    if (s.value(P.DAMAGE) > Stats.maxDamage)
		Stats.maxDamage = s.value(P.DAMAGE);
	    if (s.value(P.DAMAGE) < Stats.minDamage)
		Stats.minDamage = s.value(P.DAMAGE);
	}
    }
    
    public void classifySite(Site site) {
	if (!processedExploreValues)
	    site.setExploreValue();
	
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
	if (low == high)
	    return 1;
	return ((x - low) / (high - low));
    }
    
    public void scoreUnexplored() {
	mapScaling = (1 - ((float)unexplored.size() / Stats.totalSites));
	
	if (unexplored.size() == 0)
	    return;
	
	Collections.sort(unexplored, CompareUtil.maxProperty(P.EXPLORE_VALUE));
	
	Stats.minExplore = unexplored.get(unexplored.size() - 1).value(P.EXPLORE_VALUE);
        Stats.maxExplore = unexplored.get(0).value(P.EXPLORE_VALUE);

	ArrayList<Site> objectivePoints = new ArrayList<Site>();

	for (Site s : unexplored) {
	    if (normalize(s.value(P.EXPLORE_VALUE), Stats.minExplore, Stats.maxExplore) > Parameters.objectiveThreshold) {
		if (s.value(P.EXPLORE) < s.value(P.EXPLORE_VALUE))
		    s.set(P.EXPLORE, s.value(P.EXPLORE_VALUE));
		float v = s.value(P.EXPLORE) * 0.98f;
		for (Site n : s.neighbors.values())
		    if ((v >= n.value(P.EXPLORE)))
			n.set(P.EXPLORE, v);
		objectivePoints.add(s);
	    }
	}

	RingIterator riop = new RingIterator(objectivePoints,
					     new Predicate<Site>() {
						 @Override
						 public boolean test(Site t) {
						     return (t.value(P.EXPLORE) != 0);
						 }
					     });

	while (riop.hasNext()) {
	    boolean changed = false;
	    ArrayList<Site> sortedNeighbors = riop.next();
	    Collections.sort(sortedNeighbors, CompareUtil.maxProperty(P.EXPLORE));
	    for (Site s : sortedNeighbors) {
		float absorb = s.value(P.EXPLORE) * (0.98f - (Parameters.objectiveUnitSpread * (s.units / Site.MAX_STRENGTH)) - (Parameters.objectiveGeneratorSpread * (1 - (s.generator / Stats.maxGenerator))));
		for (Site sn : s.neighbors.values()) {
		    if ((absorb > sn.value(P.EXPLORE))) {
			sn.set(P.EXPLORE, absorb);
			changed = true;
		    }
		}
	    }
	    if (!changed)
	    	break;
	}
	
	for (Site s : sites)
	    if (!s.get(State.UNEXPLORED))
		s.set(P.EXPLORE, 0f);
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
