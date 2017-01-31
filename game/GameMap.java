package game;

import java.util.ArrayList;
import java.util.BitSet;
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
import logic.util.MathUtil;
import logic.util.RingIterator;

public class GameMap{
    public static final float MAX_SIZE = 50f;

    public Site[] sites;
    public int width;
    public int height;
    public float scale;
    public float scaling = 0;

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
	this.scale = (Math.min(width, height) - 1) * 0.5f;
	Stats.totalSites = width * height;
	sites = new Site[width * height];
	for(int x = 0; x < width; x++)
            for(int y = 0; y < height; y++)
		sites[y + (height * x)] = new Site(x, y, height);
    }

    public void addUnexplored(Site s) {
	unexplored.add(s);
        Stats.totalUnexploredGenerator += s.value(P.GENERATOR);
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
	if (site.get(State.MINE) || site.get(State.ENEMY)) {
	    if (site.units == 0)
		site.set(State.USED);
	    else if (site.get(State.ENEMY)) {
		site.set(State.ATTACK);
		for (Site n : site.neighbors.values()) {
		    n.set(State.ATTACK);
		    for (Site nn : n.neighbors.values())
			nn.set(State.ATTACK);
		}
	    }
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
		for (Site neighbor : site.neighbors.values()) {
		    if (neighbor.get(State.MINE))
			bot.addBattle(neighbor);
		    else if (neighbor.get(State.ENEMY))
			getEnemy(neighbor.owner).addBattle(neighbor);
		    if (neighbor.owner != 0)
			neighborCheck.put(neighbor.owner, true);
		}
		if (neighborCheck.size() == 1)
		    site.set(State.OPEN);
		site.set(State.BATTLE);
	    } else {
	        ArrayList<Site> frontier = new ArrayList<Site>();
		ArrayList<Site> frontierEnemy = new ArrayList<Site>();
		for (Site neighbor : site.neighbors.values())
		    if (neighbor.get(State.MINE))
			frontier.add(neighbor);
		    else if (neighbor.get(State.ENEMY))
			frontierEnemy.add(neighbor);
		if ((frontier.size() > 0) && (frontierEnemy.size() > 0)) {
		    for (Site s : frontier)
			bot.addGate(s);
		    for (Site s : frontierEnemy)
			getEnemy(s.owner).addGate(s);
		    site.set(State.GATE);
		} else {
		    if (frontier.size() > 0)
			bot.addFrontier(site);
		    else if (frontierEnemy.size() > 0) {
			BitSet used = new BitSet();
			for (Site s : frontierEnemy)
			    if (!used.get(s.owner)) {
				used.set(s.owner);
				getEnemy(s.owner).addFrontier(site);
			    }
		    }
		    addUnexplored(site);
		}
	    }
	}
    }

    public void analyzeUnexplored() {
	Site.MAX_STRENGTH_LOSSY = Site.MAX_STRENGTH + Stats.maxGenerator;
	for (Site s : sites) {
	    RingIterator ri = new RingIterator(s);
	    float total = s.value(P.GENERATOR);
	    for (int d = 0; d < Parameters.sitePotentialDistance && ri.hasNext(); d++) 
		for (Site r : ri.next()) 
		    total += (r.value(P.GENERATOR) / Stats.maxGenerator) * (1f - (Parameters.sitePotentialWeighting * (1 + d)));
	    s.sitePotential = total / Stats.totalGenerator;
	    if (s.sitePotential > Stats.maxSitePotential)
		Stats.maxSitePotential = s.sitePotential;
	}

	for (Site s : sites) {
	    float v = s.generateExploreValue();
	    if (v > Stats.maxExploreValue)
		Stats.maxExploreValue = v;
	    if ((v < Stats.minExploreValue) && (v != 0))
		Stats.minExploreValue = v;
	}

	for (Site s : sites)
	    s.set(P.EXPLORE_VALUE, MathUtil.normalize(s.value(P.EXPLORE_VALUE), Stats.minExploreValue, Stats.maxExploreValue));
    }
    
    public void scoreUnexplored() {
	scaling = (1 - ((float)unexplored.size() / Stats.totalSites));
	
	if (unexplored.size() == 0)
	    return;
	
	Collections.sort(unexplored, CompareUtil.maxProperty(P.EXPLORE_VALUE));
	
	ArrayList<Site> objectivePoints = new ArrayList<Site>();

	float total = 0f;
	for (Site s : unexplored)
	    total += s.value(P.EXPLORE_VALUE);
	total *= Parameters.objectiveThreshold;
	
	for (Site s : unexplored) {	   
	    if (total > 0) {
		total -= s.value(P.EXPLORE_VALUE);
		if (s.value(P.EXPLORE) < s.value(P.EXPLORE_VALUE))
		    s.set(P.EXPLORE, s.value(P.EXPLORE_VALUE));
		float v = s.value(P.EXPLORE) * 0.95f;
		for (Site n : s.neighbors.values())
		    if ((v >= n.value(P.EXPLORE)))
			n.set(P.EXPLORE, v);
		objectivePoints.add(s);
	    } else
		break;
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
		float absorb = s.value(P.EXPLORE) * (0.98f - (Parameters.objectiveUnitSpread * (s.units / Site.MAX_STRENGTH)) - (Parameters.objectiveGeneratorSpread * (1 - (s.value(P.GENERATOR) / Stats.maxGenerator))));
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
	    if (!s.get(State.UNEXPLORED) || (s.value(P.GENERATOR) == 0))
		s.set(P.EXPLORE, 0);
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
