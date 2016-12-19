package game;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.function.Predicate;

import bot.AI;
import bot.model.Enemy;
import bot.model.Entity;
import bot.util.RingIterator;

import game.Site.State;

public class GameMap{
    public Site[] sites;
    public int width;
    public int height;
    public float scaler;
    public int totalSites;

    public AI bot;

    public HashMap<Byte, Enemy> enemies = new HashMap<Byte, Enemy>();
    
    public HashSet<Site> unexplored = new HashSet<Site>();

    private Predicate<Site> p = new Predicate<Site>() {
	    @Override
	    public boolean test(Site s) {
		return (s.get(State.MINE) && s.get(State.INTERIOR)) || s.get(State.BATTLE);
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
	this.scaler = Math.min(width, height) * 0.5f;
	this.totalSites = width * height;
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
    
    public void analyzeSites() {
	for (int i = 0; i < sites.length; i++)
	    findSpearPoints(sites[i]);
    }
    
    private void findSpearPoints(Site site) {
	if (site.get(State.MINE)) {
	    RingIterator ri = new RingIterator(site, p);
	    if (ri.next().size() == 0)
		bot.addSpear(site);
	}
    }
    
    public void classifySite(Site site) {
	if (site.get(State.MINE) || site.get(State.ENEMY)) {
	    if (site.aboveActionThreshold())
		site.set(State.READY);
	    
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
			    bot.addField(site);
			else if (neighbor.get(State.ENEMY)) 
			    getEnemy(neighbor.owner).addField(site);
		}
		site.set(State.BATTLE);
	    } else {
		boolean frontier = false;
		boolean frontierEnemy = false;
		boolean alreadyFighting = false;
		for (Site neighbor : site.neighbors.values()) {
		    if (neighbor.get(State.MINE))
			frontier = true;
		    else if (neighbor.get(State.ENEMY))
			frontierEnemy = true;
		    else if (neighbor.get(State.NEUTRAL) && (neighbor.units == 0))
			alreadyFighting = true;
		}
		if (frontier && frontierEnemy && !alreadyFighting) {
		    for (Site neighbor : site.neighbors.values())
			if (neighbor.get(State.MINE))
			    bot.addBattle(neighbor);
			else if (neighbor.get(State.ENEMY))
			    getEnemy(neighbor.owner).addBattle(neighbor);
		    site.set(State.BATTLE);
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

    public void scoreUnexplored() {
	float lowest = Float.MAX_VALUE;
	float highest = 0;
	Predicate<Site> p = new Predicate<Site>() {
		@Override
		public boolean test(Site t) {
		    return t.get(State.NEUTRAL) && (t.units != 0);
		}
	    };
	ArrayList<Site> sortedUnexplored = new ArrayList<Site>(unexplored.size());
	for (Site s : unexplored) {
	    if ((s.units != 0) && (s.generator > 0)) {
		RingIterator ri = new RingIterator(s, p);
		float totalWeight = 1f;
		float total = s.getExploreValue();
		for (int d = 0; d < 3 && ri.hasNext(); d++) {
		    HashSet<Site> ring = ri.next();
		    for (Site r : ring) 
			total += r.getExploreValue();
		    totalWeight += ring.size() * (1 - (0.3f * d));
		}
		s.explore = total / totalWeight;
		sortedUnexplored.add(s);
		if ((s.explore != 0) && (s.explore < lowest))
		    lowest = s.explore;
		if (s.explore > highest)
		    highest = s.explore;
	    }
	}
	Comparator<Site> c = new Comparator<Site>() {
		@Override
		public int compare(Site arg0, Site arg1) {
		    float v = arg1.explore - arg0.explore;
		    if (v == 0)
			return arg0.id - arg1.id;
		    return v > 0 ? 1 : -1;
		}
	    };
	
	Collections.sort(sortedUnexplored, c);
	for (Iterator<Site> cursor = sortedUnexplored.iterator(); cursor.hasNext();) {
	    Site s = cursor.next();
	    if (((s.explore - lowest) / (highest - lowest)) < 0.30) {
		s.explore = 0;
		cursor.remove();
	    }
	}

	lowest = Float.MAX_VALUE;
	highest = 0;
	for (Site s : sortedUnexplored) {
	    RingIterator sri = new RingIterator(s, p);
	    HashSet<Site> currentSet = new HashSet<Site>();
	    currentSet.add(s);
	    boolean changed = true;
	    while (sri.hasNext() && changed) {
		changed = false;
		HashSet<Site> nextSet = sri.next();
		for (Site r : nextSet) {
		    highest = -Float.MAX_VALUE;
		    RingIterator rri = new RingIterator(r, p);
		    for (Site rr : rri.next()) 
			if ((currentSet.contains(rr) || (rr == s)) && (highest < rr.explore))
			    highest = rr.explore;
		    float a = highest * (0.90f - (0.5f * (r.units / Site.MAX_STRENGTH)));

		    if (a > r.explore) {
			changed = true;
			r.explore = a;
		    }
		}
		currentSet = nextSet;
	    }
	}

	for (Site s: unexplored) {
	    if (s.explore < lowest)
		lowest = s.explore;
	    if (s.explore > highest)
		highest = s.explore;		    
	}
	
	for (Site s : unexplored) {
	    if (((s.explore - lowest) / (highest - lowest)) > 0.75) {
		s.set(State.OBJECTIVE);
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
	    //enemy.placeDamageRadius();
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
