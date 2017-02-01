package logic.model;

import java.util.HashSet;

import game.Debug;
import game.GameMap;
import game.Site;
import game.Site.P;
import game.Site.State;

abstract public class Entity {
    public int id;
    public int totalGenerator = 0;
    public int totalUnits = 0;
    public int totalSites = 0;

    public GameMap map;

    public HashSet<Site> allSites = new HashSet<Site>();
    
    public Entity(int id, GameMap map) {
	this.id = id;
	this.map = map;
    }

    public String toString() {
	return id + " " + totalGenerator + " " + totalUnits + " " + totalSites;
    }
    
    private void addSite(Site s) {
	totalGenerator += s.value(P.GENERATOR);
	totalUnits += s.units;
	totalSites += 1;
	if (Debug.enabled) {
	    if (allSites.contains(s))
		Debug.abort(s.encodeSite() + " " + s.encodeAttributes());
	    allSites.add(s);
	}
    }

    public void addGate(Site s) {
	if (!s.get(State.GATE)) {
	    if (s.get(State.BORDER))
		s.remove(State.BORDER);
	    else if (s.get(State.BATTLE))
		s.remove(State.BATTLE);
	    else 
		addSite(s);
	    s.set(State.GATE);
	}
    }
       
    public void addFrontier(Site s) {
	s.set(State.FRONTIER);
    }

    public void addBorder(Site s) {
	if (!s.get(State.BATTLE) && !s.get(State.GATE) && !s.get(State.BORDER)) {
	    addSite(s);
	    s.set(State.BORDER);
	}
    }
    
    public void addBattle(Site s) {
	if (!s.get(State.BATTLE) && !s.get(State.GATE)) {
	    if (s.get(State.BORDER))
		s.remove(State.BORDER);
	    else 
		addSite(s);
	    s.set(State.BATTLE);
	}
    }

    public void addInterior(Site s) {
	s.set(State.INTERIOR);
	addSite(s);
    }

    public void reset() {
	if (Debug.enabled)
	    allSites.clear();
	totalGenerator = 0;
	totalUnits = 0;
	totalSites = 0;
    }
}
