package game;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.function.Predicate;

import logic.AI;
import logic.Constants;
import logic.Constants.F;
import logic.Constants.P;
import logic.Constants.S;
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
    
    public void reset() {
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
        Stats.totalUnexploredGenerator += s.value(P.GENERATOR);
	s.set(S.UNEXPLORED);
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
	    if (s.value(F.EXPLORE) > Stats.maxExplore)
		Stats.maxExplore = s.value(F.EXPLORE);
	    if (s.value(F.EXPLORE) < Stats.minExplore)
		Stats.minExplore = s.value(F.EXPLORE);
	    if (s.value(F.REINFORCE) > Stats.maxReinforce)
		Stats.maxReinforce = s.value(F.REINFORCE);
	    if (s.value(F.REINFORCE) < Stats.minReinforce)
		Stats.minReinforce = s.value(F.REINFORCE);
	    if (s.value(F.DAMAGE) > Stats.maxDamage)
		Stats.maxDamage = s.value(F.DAMAGE);
	    if (s.value(F.DAMAGE) < Stats.minDamage)
		Stats.minDamage = s.value(F.DAMAGE);
	}
    }
    
    public void classifySite(Site site) {
	if (site.get(S.MINE) || site.get(S.ENEMY)) {
	    if (site.units == 0)
		site.set(S.USED);
	    else if (site.get(S.ENEMY)) {
		site.set(S.ATTACK);
		for (Site n : site.neighbors.values()) {
		    n.set(S.ATTACK);
		    for (Site nn : n.neighbors.values())
			nn.set(S.ATTACK);
		}
	    }
	    if (site.aboveActionThreshold())
		site.set(S.READY);
	    if (site.aboveCombatThreshold())
		site.set(S.COMBAT_READY);
	    
	    Entity e;
	    if (site.get(S.MINE))
		e = bot;
	    else 
		e = getEnemy(site.owner);
	    
	    boolean border = false;
	    for (Site neighbor : site.neighbors.values())
		if (neighbor.get(S.NEUTRAL)) {
		    border = true;
		    break;
		} 
	    
	    if (!border)
		e.addInterior(site);
	    else
		e.addBorder(site);
	} else if (site.get(S.NEUTRAL)) {
	    if (site.units == 0) {
		HashMap<Integer, Boolean> neighborCheck = new HashMap<Integer, Boolean>();
		for (Site neighbor : site.neighbors.values()) {
		    if (neighbor.get(S.MINE))
			bot.addBattle(neighbor);
		    else if (neighbor.get(S.ENEMY))
			getEnemy(neighbor.owner).addBattle(neighbor);
		    if (neighbor.owner != 0)
			neighborCheck.put(neighbor.owner, true);
		}
		if (neighborCheck.size() == 1)
		    site.set(S.OPEN);
		site.set(S.BATTLE);
	    } else {
	        ArrayList<Site> frontier = new ArrayList<Site>();
		ArrayList<Site> frontierEnemy = new ArrayList<Site>();
		for (Site neighbor : site.neighbors.values())
		    if (neighbor.get(S.MINE))
			frontier.add(neighbor);
		    else if (neighbor.get(S.ENEMY))
			frontierEnemy.add(neighbor);
		if ((frontier.size() > 0) && (frontierEnemy.size() > 0)) {
		    for (Site s : frontier)
			bot.addGate(s);
		    for (Site s : frontierEnemy)
			getEnemy(s.owner).addGate(s);
		    site.set(S.GATE);
		} else {
		    if (frontier.size() > 0)
			bot.addFrontier(site);
		    else if (frontierEnemy.size() > 0) {
			// BitSet used = new BitSet();
			// for (Site s : frontierEnemy)
			//     if (!used.get(s.owner)) {
			// 	used.set(s.owner);
			// 	getEnemy(s.owner).addFrontier(site);
			//     }
		    }
		    addUnexplored(site);
		}
	    }
	}
    }

    public void prepSites() {
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
	ArrayList<Site> unexplored = new ArrayList<Site>((int)Stats.totalSites);
	for (Site s : sites)
	    if (s.get(S.UNEXPLORED))
		unexplored.add(s);
	
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
		if (s.value(F.EXPLORE) < s.value(P.EXPLORE_VALUE))
		    s.set(F.EXPLORE, s.value(P.EXPLORE_VALUE));
		float v = s.value(F.EXPLORE) * 0.95f;
		for (Site n : s.neighbors.values())
		    if ((v >= n.value(F.EXPLORE)))
			n.set(F.EXPLORE, v);
		objectivePoints.add(s);
	    } else
		break;
	}
	RingIterator riop = new RingIterator(objectivePoints,
					     new Predicate<Site>() {
						 @Override
						 public boolean test(Site t) {
						     return (t.value(F.EXPLORE) != 0);
						 }
					     });

	while (riop.hasNext()) {
	    boolean changed = false;
	    ArrayList<Site> sortedNeighbors = riop.next();
	    Collections.sort(sortedNeighbors, CompareUtil.maxField(F.EXPLORE));
	    for (Site s : sortedNeighbors) {
		float absorb = s.value(F.EXPLORE) * (0.98f - (Parameters.objectiveUnitSpread * (s.units / Constants.MAX_UNITS)) - (Parameters.objectiveGeneratorSpread * (1 - (s.value(P.GENERATOR) / Stats.maxGenerator))));
		for (Site sn : s.neighbors.values()) {
		    if ((absorb > sn.value(F.EXPLORE))) {
			sn.set(F.EXPLORE, absorb);
			changed = true;
		    }
		}
	    }
	    if (!changed)
	    	break;
	}

	// for (int i = 0; i < 8; i++) {
	//     for (Site s : unexplored) {
	// 	float totalExplore = 0f;
	// 	float value = s.v(F.EXPLORE);
	// 	for (Site n : s.neighbors.values())
	// 	    if (n.is(S.UNEXPLORED))
	// 		totalExplore += n.v(F.EXPLORE) - value;
	// 	s.stage(F.EXPLORE, (1.35f - (Parameters.objectiveUnitSpread * (s.v(P.UNITS) / Constants.MAX_UNITS)) - (Parameters.objectiveGeneratorSpread * (1 - (s.v(P.GENERATOR) / Stats.maxGenerator)))) * (s.v(F.EXPLORE) + (0.25f * totalExplore)));
	//     }
	//     for (Site s : sites)
	// 	s.commit();

	
	for (Site s : sites)
	    if (!s.get(S.UNEXPLORED) || (s.value(P.GENERATOR) == 0))
		s.set(F.EXPLORE, 0);
    }
	

    public int safeCoordinate(int x, int limit) {
	if (x < 0) 
	    return limit + (x % limit);
	else
	    return x % limit;
    }

    public void identifyEnemy() {
	for (Entry<Integer, Enemy> e : enemies.entrySet())
	    e.getValue().placeDefense();
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
