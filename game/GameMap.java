package game;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;

import bot.AI;
import bot.model.Enemy;

public class GameMap{
    public static final byte MAX_SIZE = 50;
    public static final byte MIN_SIZE = 15;

    private Site[] sites;
    public int width;
    public int height;
    public int totalSites;

    public AI bot;

    public HashMap<Byte, Enemy> enemies = new HashMap<Byte, Enemy>();
    
    public HashSet<Site> unexplored = new HashSet<Site>();
    public ArrayList<Site> battles = new ArrayList<Site>();
    public HashSet<Site> fields = new HashSet<Site>();
    
    private void reset() {
	battles.clear();
	unexplored.clear();
	fields.clear();
	Stats.reset();
	bot.reset();
	for (Entry<Byte, Enemy> e : enemies.entrySet())
	    e.getValue().reset();
    }

    public void buildSites(byte width, byte height) {
        this.width = width;
        this.height = height;
	this.totalSites = width * height;
        sites = new Site[width * height];
        for(byte x = 0; x < width; x++)
            for(byte y = 0; y < height; y++)
		sites[y + (height * x)] = new Site(x, y, height);
    }

    public void addUnexplored(Site s) {
	unexplored.add(s);
        Stats.totalUnexploredGenerator += s.generator;
	s.set(Site.State.UNEXPLORED);
    }

    public void addBattle(Site s) {
        battles.add(s);
	s.set(Site.State.BATTLE);
    }

    public void addField(Site s) {
        fields.add(s);
	s.set(Site.State.FIELD);
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

    public Site getSite(int i) {
	return sites[i];
    }

    public void collectStats() {
	for (int i = 0; i < sites.length; i++) {
	    Site s = sites[i];
	    if (s.explore > Stats.maxExplore)
		Stats.maxExplore = s.explore;
	    if (s.explore < Stats.minExplore)
		Stats.minExplore = s.explore;
	    if (s.strength > Stats.maxStrength)
		Stats.maxStrength = s.strength;
	    if (s.strength < Stats.minStrength)
		Stats.minStrength = s.strength;
	    if (s.damage > Stats.maxDamage)
		Stats.maxDamage = s.damage;
	    if (s.damage < Stats.minDamage)
		Stats.minDamage = s.damage;
	    if (s.defense > Stats.maxDefense)
		Stats.maxDefense = s.defense;
	    if (s.defense < Stats.minDefense)
		Stats.minDefense = s.defense;
	    if (s.strategy > Stats.maxStrategy)
		Stats.maxStrategy = s.strategy;
	    if (s.strategy < Stats.minStrategy)
		Stats.minStrategy = s.strategy;
	}
    }
    
    public void commitChanges() {
	reset();
	for (int i = 0; i < sites.length; i++)
	    analyzeSite(sites[i]);
    }

    private void analyzeSite(Site site) {
	site.owner = site.newOwner;
	site.units = site.newUnits;
	site.reset();
	if (site.owner == 0)
	    site.set(Site.State.NEUTRAL);
	else if (site.owner == bot.id)
	    site.set(Site.State.MINE);
	else
	    site.set(Site.State.ENEMY);
	
	if (site.get(Site.State.MINE) || site.get(Site.State.ENEMY)) {
	    if (site.get(Site.State.MINE) && (site.units == 0))
		site.set(Site.State.USED);
	    
	    boolean border = false;
	    for (Site neighbor : site.neighbors.values())
		if (neighbor.get(Site.State.NEUTRAL)) {
		    border = true;
		    break;
		}

	    if (site.get(Site.State.MINE)) {
		if (site.aboveActionThreshold())
		    site.set(Site.State.READY);
		if (!border)
		    bot.addInterior(site);
		else
		    bot.addBorder(site);
	    } else {
		if (!border)
		    getEnemy(site.owner).addInterior(site);
		else
		    getEnemy(site.owner).addBorder(site);
	    }
	} else if (site.get(Site.State.NEUTRAL)) {
	    if (site.units == 0) {
		boolean field = false;
		boolean enemy = false;
	    	for (Site neighbor : site.neighbors.values())
	    	    if (neighbor.get(Site.State.MINE)) {
	    		bot.addBattle(neighbor);
			field = true;
		    } else if (neighbor.get(Site.State.ENEMY)) {
	    		getEnemy(neighbor.owner).addBattle(neighbor);
			enemy = true;
		    }
		if (field && !enemy)
		    addField(site);
		addBattle(site);
	    } else {
	    	boolean frontier = false;
	    	boolean frontierEnemy = false;
	    	for (Site neighbor : site.neighbors.values()) {
	    	    if (neighbor.get(Site.State.MINE))
	    		frontier = true;
		    else if (neighbor.get(Site.State.ENEMY))
	    		frontierEnemy = true;
	    	}
		if (frontier && frontierEnemy) {
		    for (Site neighbor : site.neighbors.values())
			if (neighbor.get(Site.State.MINE))
			    bot.addBattle(neighbor);
			else if (neighbor.get(Site.State.ENEMY))
			    getEnemy(neighbor.owner).addBattle(neighbor);
		    addBattle(site);
		} else {
		    for (Site neighbor : site.neighbors.values())
			if (neighbor.get(Site.State.MINE))
			    bot.addFrontier(site);
			else if (neighbor.get(Site.State.ENEMY))
			    getEnemy(neighbor.owner).addFrontier(site);
		    addUnexplored(site);
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

    public void postAnalyzeEnemies() {
	for (Entry<Byte, Enemy> e : enemies.entrySet()) {
	    Enemy enemy = e.getValue();
	    enemy.postProcess();
	}
    }

    public void analyzeEnemies() {
	for (Entry<Byte, Enemy> e : enemies.entrySet()) {
	    Enemy enemy = e.getValue();
	    enemy.placeDamageRadius();
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
