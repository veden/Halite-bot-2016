package bot.model;

import java.util.ArrayList;

import game.GameMap;
import game.Site;
import game.Site.State;

abstract public class Entity {
    public byte id;
    public int totalGenerator = 0;
    public int totalUnits = 0;
    public int totalSites = 0;

    public int totalPotentialGeneration = 0;
    public int totalGeneratedPotential = 0;
    public int totalOverkill = 0;
    public int totalDamage = 0;
    public int totalCappedLoss = 0;
    public int totalCaptured = 0;

    public GameMap map;

    // public HashSet<Site> allSites = new HashSet<Site>();
    public ArrayList<Site> battles = new ArrayList<Site>();
    public ArrayList<Site> interior = new ArrayList<Site>();
    public ArrayList<Site> border = new ArrayList<Site>();
    public ArrayList<Site> frontier = new ArrayList<Site>();
    public ArrayList<Site> spears = new ArrayList<Site>();
    public ArrayList<Site> open = new ArrayList<Site>();
    public ArrayList<Site> gates = new ArrayList<Site>();
    
    public Entity(byte id, GameMap map) {
	this.id = id;
	this.map = map;
    }

    public String toString() {
	return id + " " + totalGenerator + " " + totalUnits + " " + totalPotentialGeneration + " " + totalGeneratedPotential + " " + totalOverkill + " " + totalDamage + " " + totalCappedLoss + " " + totalCaptured + " " + totalSites;
    }
    
    private void addSite(Site s) {
	totalGenerator += s.generator;
	totalUnits += s.units;
	totalSites += 1;
	// if (allSites.contains(s))
	//     Debug.abort(s.encodeString());
	//allSites.add(s);
    }

    public void addGate(Site s) {
	gates.add(s);
	s.set(State.GATE);
    }
    
    public void addOpen(Site s) {
        open.add(s);
	s.set(State.OPEN);
    }

    public void addSpear(Site s) {
	if (!s.get(State.BATTLE)) {
	    if (s.get(State.BORDER)) {
		border.remove(s);
		s.remove(State.BORDER);
	    } else
		addSite(s);
	    s.set(State.SPEAR);
	    spears.add(s);
	}
    }
    
    public void addFrontier(Site s) {
	frontier.add(s);
	s.set(State.FRONTIER);
    }

    public void addBorder(Site s) {
	if (!s.get(State.BATTLE)) {
	    addSite(s);
	    border.add(s);
	    s.set(State.BORDER);
	}
    }
    
    public void addBattle(Site s) {
	if (!s.get(State.BATTLE)) {
	    if (s.get(State.BORDER)) {
		border.remove(s);
		s.remove(State.BORDER);
	    } else if (s.get(State.SPEAR)) {
		spears.remove(s);
		s.remove(State.SPEAR);
	    } else 
		addSite(s);
	    battles.add(s);
	    s.set(State.BATTLE);
	}
    }

    public void addInterior(Site s) {
	interior.add(s);
	s.set(State.INTERIOR);
	addSite(s);
    }

    public void reset() {
	// allSites.clear();
	gates.clear();
	open.clear();
	battles.clear();
	interior.clear();
	border.clear();
	frontier.clear();
	spears.clear();
	totalPotentialGeneration += totalGenerator;
	totalGenerator = 0;
	totalUnits = 0;
	totalSites = 0;
    }
}
