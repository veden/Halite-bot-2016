package game.bot.model;

import java.util.BitSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;

import game.GameMap;
import game.Harness;
import game.Site;
import game.bot.util.FloodFunction;
import game.bot.util.SiteFunction;
import game.bot.util.SiteUtils;

public class Enemy extends Entity {
    public static HashMap<Byte, Enemy> enemies = new HashMap<Byte, Enemy>();

    private static FloodFunction spreadAttackTrigger = new FloodFunction() {  
	    @Override
	    public void scan(Site neighbor, Site center) {
		if (best < neighbor.defense)
		    best = neighbor.defense;
	    }

	    @Override
	    public void process(Site s, BitSet used, Site center, float distance, HashSet<Site> current, HashSet<Site> next) {
		float v = (1 - (0.04f * distance)) * best;
		if (v > s.defense)
		    s.defense = v;
		for (Site neighbor : s.neighbors.values())
		    if ((neighbor.owner == center.owner) && (neighbor.units == 0) && !used.get(neighbor.id))
			next.add(neighbor);
	    }
	};

    private static SiteFunction identifyEnemyProduction = new SiteFunction() {
	    @Override
	    public void process(Site s, BitSet used, HashSet<Site> current, HashSet<Site> next, Site center, float distance) {
		if (s.get(Site.State.ENEMY)) {
		    totalWeight += (1 - (0.2f * distance));
		    total += s.generator;
		    if ((distance+1<3) && (!used.get(s.id))) {
			next.add(s);
			used.set(s.id);
		    }
		}
	    }
	};

    private static FloodFunction enemyAura = new FloodFunction() {

	    @Override
	    public void scan(Site neighbor, Site center) {
		if (best < neighbor.explore)
		    best = neighbor.explore;
	    }

	    @Override
	    public void process(Site s, BitSet used, Site center, float distance, HashSet<Site> current, HashSet<Site> next) {		
		float v = (1 - (0.04f * distance)) * best;
		if (v > s.explore)
		    s.explore = v;
		if (v > 0)
		    for (Site neighbor : s.neighbors.values())
			if (neighbor.get(Site.State.NEUTRAL) && !used.get(neighbor.id))
			    next.add(neighbor);
	    }	    
	};
    
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
	    i.defense = SiteUtils.scoreWeighted(i, map, identifyEnemyProduction.setInitial(i.generator));
	for (Site b : border)
	    b.defense = SiteUtils.scoreWeighted(b, map, identifyEnemyProduction.setInitial(b.generator));
	for (Site c : battles) {
	    c.defense = SiteUtils.scoreWeighted(c, map, identifyEnemyProduction.setInitial(c.generator));
	    for (Site neighbor : c.neighbors.values())
		if (neighbor.get(Site.State.BATTLE) && neighbor.get(Site.State.NEUTRAL)) {
		    neighbor.defense = 0.9f * c.defense;
		    if (neighbor.units == 0)
			SiteUtils.flood(neighbor, map, spreadAttackTrigger.setInitial(neighbor.defense));
		}
	}    
    }

    public void postProcess() {
	// for (Site b : border) {
	//     for (Site s : b.neighbors.values())
	// 	if (s.get(Site.State.NEUTRAL))
	// 	    SiteUtils.flood(s, map, enemyAura.setInitial(s.explore));

	// }
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

    public static String toReplay() {
	StringBuilder sb = new StringBuilder();
	for (Entry<Byte, Enemy> e : enemies.entrySet()) {
	    if (sb.length() > 0)
		sb.append("\n");
	    sb.append(e.getValue().toString());
	}
	return sb.toString();
    }
}
