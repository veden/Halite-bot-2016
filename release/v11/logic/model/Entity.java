package logic.model;

import java.util.ArrayList;
import java.util.HashSet;

import game.Debug;
import game.GameMap;
import game.Site;
import game.Site.State;

abstract public class Entity {
    public int id;
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

    public HashSet<Site> allSites = new HashSet<Site>();
    public ArrayList<Site> battles = new ArrayList<Site>();
    public ArrayList<Site> interior = new ArrayList<Site>();
    public ArrayList<Site> border = new ArrayList<Site>();
    public ArrayList<Site> frontier = new ArrayList<Site>();
    public ArrayList<Site> open = new ArrayList<Site>();
    public ArrayList<Site> gates = new ArrayList<Site>();
    
    public Entity(int id, GameMap map) {
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
	if (Debug.enabled) {
	    if (allSites.contains(s))
		Debug.abort(s.encodeSite() + " " + s.encodeAttributes());
	    allSites.add(s);
	}
    }

    public void addGate(Site s) {
	if (s.get(State.BORDER)) {
	    border.remove(s);
	    s.remove(State.BORDER);
	    gates.add(s);
	    s.set(State.GATE);
	} else if (s.get(State.BATTLE)) {
	    battles.remove(s);
	    s.remove(State.BATTLE);
	    gates.add(s);
	    s.set(State.GATE);
	}
	if (!s.get(State.GATE)) {
	    addSite(s);
	    gates.add(s);
	    s.set(State.GATE);
	} 
    }
    
    public void addOpen(Site s) {
	open.add(s);
	s.set(State.OPEN);
    }
    
    public void addFrontier(Site s) {
	frontier.add(s);
	s.set(State.FRONTIER);
    }

    public void addBorder(Site s) {
	if (!s.get(State.BATTLE) && !s.get(State.GATE) && !s.get(State.BORDER)) {
	    addSite(s);
	    border.add(s);
	    s.set(State.BORDER);
	}
    }
    
    public void addBattle(Site s) {
	if (!s.get(State.BATTLE) && !s.get(State.GATE)) {
	    if (s.get(State.BORDER)) {
		border.remove(s);
		s.remove(State.BORDER);
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
	if (Debug.enabled)
	    allSites.clear();
	gates.clear();
	open.clear();
	battles.clear();
	interior.clear();
	border.clear();
	frontier.clear();
	totalPotentialGeneration += totalGenerator;
	totalGenerator = 0;
	totalUnits = 0;
	totalSites = 0;
    }
}
