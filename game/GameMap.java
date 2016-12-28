package game;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.function.Predicate;

import bot.AI;
import bot.model.Enemy;
import bot.model.Entity;
import bot.util.RingIterator;

import game.Site.State;

public class GameMap{
    public static final float MAX_SIZE = 50f;

    private boolean foundObjectives = false;
    public Site[] sites;
    public int width;
    public int height;
    public float scaler;

    public AI bot;

    public HashMap<Byte, Enemy> enemies = new HashMap<Byte, Enemy>();
    
    public HashSet<Site> unexplored = new HashSet<Site>();

    private Predicate<Site> p = new Predicate<Site>() {
	    @Override
	    public boolean test(Site s) {
		return s.get(State.INTERIOR) || s.get(State.BATTLE);
	    }
	};
    
    public void reset() {
	unexplored.clear();
	Stats.reset();
	bot.reset();
	for (Entry<Byte, Enemy> e : enemies.entrySet())
	    e.getValue().reset();
    }

    public void buildSites(byte width, byte height) {
        this.width = width;
        this.height = height;
	this.scaler = (Math.min(width, height) - 1) * 0.5f;
	Stats.totalSites = width * height;
	sites = new Site[width * height];
	for(byte x = 0; x < width; x++)
            for(byte y = 0; y < height; y++)
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
		HashMap<Byte, Boolean> neighborCheck = new HashMap<Byte, Boolean>();
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
	float minExplore = Float.MAX_VALUE;
	float maxExplore = 0;
	Predicate<Site> p = new Predicate<Site>() {
		@Override
		public boolean test(Site t) {
		    return t.get(State.UNEXPLORED) || (t.get(State.MINE));
		}
	    };

	// for (Site s: unexplored) {
	//     if (s.explore < minExplore)
	// 	minExplore = s.explore;
	//     if (s.explore > maxExplore)
	// 	maxExplore = s.explore;		    
	// }	
	// for (Site s : unexplored) 
	//     if ((s.units != 0) && (s.generator > 0)) {
	// 	RingIterator sri = new RingIterator(s, p);
	// 	int d = 0;
	// 	float value = s.getExploreValue();
	// 	if (value > s.explore)
	// 	    s.explore = value;
	// 	float scale = 0.50f;
	// 	float score = normalize(s.explore, minExplore, maxExplore);
	// 	if (score > 0.65)
	// 	    scale = 0.40f;
	// 	else if (score <= 0.40f)
	// 	    continue;
	// 	float unitScale = scale * 0.5f;
	// 	float generatorScale = scale * 0.5f;
	// 	while (sri.hasNext() && (d < 10)) {
	// 	    d++;
	// 	    for (Site r : sri.next()) {
	// 		float highest = -Float.MAX_VALUE;
	// 		for (Site rr : r.neighbors.values()) 
	// 		    if (rr.get(State.UNEXPLORED) && (highest < rr.explore)) 
	// 			highest = rr.explore;
	// 		float v = highest * (0.98f - (unitScale * (r.units / Site.MAX_STRENGTH)) - (generatorScale * (1 - (r.generator / Stats.maxGenerator))));
	// 		if (v > r.explore)
	// 		    r.explore = v;
	// 	    }		
	// 	}
	//     } 
	
	
	for (Site s: unexplored) {
	    if (s.explore < minExplore)
		minExplore = s.explore;
	    if (s.explore > maxExplore)
		maxExplore = s.explore;		    
	}	
	for (Site s : unexplored) 
	    if ((s.units != 0) && (s.generator > 0)) {
		RingIterator sri = new RingIterator(s, p);
		int d = 0;
		float value = s.getExploreValue();
		if (value > s.explore)
		    s.explore = value;
		float scale = 0.75f;
		// float score = normalize(value, minExplore, maxExplore);
		// if (score < 0.65)
		//     continue;
		float unitScale = scale * 0.75f;
		float generatorScale = scale * 0.25f;
		while (sri.hasNext() && (d < 15)) {
		    d++;
		    for (Site r : sri.next()) {
			if (r.get(State.UNEXPLORED)) {
			    float v = value * (0.95f - (0.04f * (d + 1)) - (unitScale * (r.units / Site.MAX_STRENGTH)) - (generatorScale * (1 - (r.generator / Stats.maxGenerator))));
			    if (v > r.explore)
				r.explore = v;
			} else {
			    float v = value * (0.95f - (0.04f * (d + 1)));
			    if (v > r.reinforce)
				r.reinforce = v;
			}
		    }		
		}
	    }
    }

    public short safeCoordinate(int x, int limit) {
	if (x < 0) 
	    return (short)(limit + (x % limit));
	else
	    return (short)(x % limit);
    }

    public void identifyEnemy() {
	for (Entry<Byte, Enemy> e : enemies.entrySet()) {
	    Enemy enemy = e.getValue();
	    enemy.placeDefense();
	}
    }
    
    public Enemy getEnemy(byte id) {
	if (!enemies.containsKey(id))
	    enemies.put(id, new Enemy(id, this));
	return enemies.get(id);
    }

    public String enemiesToReplay() {
	StringBuilder sb = new StringBuilder();
	for (Entry<Byte, Enemy> e : enemies.entrySet()) {
	    if (sb.length() > 0)
		sb.append("\n");
	    sb.append(e.getValue().toString());
	}
	return sb.toString();
    }
}
