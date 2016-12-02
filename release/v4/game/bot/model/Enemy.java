package game.bot.model;

import java.util.HashMap;
import java.util.Map.Entry;

import game.GameMap;
import game.Harness;
import game.Site;
import game.bot.SiteUtils;

public class Enemy extends Entity {
    public static HashMap<Byte, Enemy> enemies = new HashMap<Byte, Enemy>();

    public Enemy(byte id, GameMap map) {
	super(id, map);
    }
    
    public void placeDamageRadius() {
	for (Site b : border)
	    SiteUtils.spreadDamage(b);
	for (Site b : battles)
	    SiteUtils.spreadDamage(b);
    }

    public void placeDefense() {
	for (Site i : interior)
	    i.defense = SiteUtils.scoreGenerator(i, map);
	for (Site b : border) {
	    b.defense = SiteUtils.scoreGenerator(b, map);
	}
	for (Site c : battles) {
	    c.defense = SiteUtils.scoreGenerator(c, map);
	    for (Site neighbor : c.neighbors.values())
		if (neighbor.get(Site.State.BATTLE) && neighbor.get(Site.State.NEUTRAL)) {
		    neighbor.defense = 0.9f * c.defense;
		    if (neighbor.units == 0)
			SiteUtils.floodFillDefense(neighbor, map, false);
		}
	}    
    }

    public void postProcess() {
	for (Site b : border) {
	    for (Site s : b.neighbors.values())
		if (s.get(Site.State.NEUTRAL))
		    SiteUtils.floodReduceExplore(s);
	}
    }
    
    public static void postAnalyze() {
	for (Entry<Byte, Enemy> e : enemies.entrySet()) {
	    Enemy enemy = e.getValue();
	    enemy.postProcess();
	}
    }

    public static void analyze() {
	for (Entry<Byte, Enemy> e : enemies.entrySet()) {
	    Enemy enemy = e.getValue();
	    enemy.placeDamageRadius();
	    enemy.placeDefense();
	}
    }
    
    public static Enemy get(byte id) {
	if (!enemies.containsKey(id))
	    enemies.put(id, new Enemy(id, Harness.map));
	return enemies.get(id);
    }

    public static void resetAll() {
	for (Entry<Byte, Enemy> e : enemies.entrySet())
	    e.getValue().reset();
    }
}
