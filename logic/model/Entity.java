package logic.model;

import java.util.HashSet;

import game.Debug;
import game.GameMap;
import game.Site;

import logic.Constants.P;
import logic.Constants.S;

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
	if (!s.get(S.GATE)) {
	    if (s.get(S.BORDER))
		s.remove(S.BORDER);
	    else if (s.get(S.BATTLE))
		s.remove(S.BATTLE);
	    else 
		addSite(s);
	    s.set(S.GATE);
	}
    }
       
    public void addFrontier(Site s) {
	s.set(S.FRONTIER);
    }

    public void addBorder(Site s) {
	if (!s.get(S.BATTLE) && !s.get(S.GATE) && !s.get(S.BORDER)) {
	    addSite(s);
	    s.set(S.BORDER);
	}
    }
    
    public void addBattle(Site s) {
	if (!s.get(S.BATTLE) && !s.get(S.GATE)) {
	    if (s.get(S.BORDER))
		s.remove(S.BORDER);
	    else 
		addSite(s);
	    s.set(S.BATTLE);
	}
    }

    public void addInterior(Site s) {
	s.set(S.INTERIOR);
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
