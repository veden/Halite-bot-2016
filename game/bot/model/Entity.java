package game.bot.model;

import java.util.HashSet;

import game.GameMap;
import game.Site;

abstract public class Entity {
    public byte id;
    public short totalGenerator = 0;
    public short totalUnits = 0;
    public short totalSites = 0;

    public int totalPotentialGeneration = 0;
    public int totalGeneratedPotential = 0;
    public int totalOverkill = 0;
    public int totalDamage = 0;
    public int totalCappedLoss = 0;
    public int totalCaptured = 0;

    public GameMap map;
    
    public HashSet<Site> battles = new HashSet<Site>();
    public HashSet<Site> interior = new HashSet<Site>();
    public HashSet<Site> border = new HashSet<Site>();
    public HashSet<Site> frontier = new HashSet<Site>();
    
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
    }

    public void addFrontier(Site s) {
	frontier.add(s);
	s.set(Site.State.FRONTIER);
    }

    public void addBorder(Site s) {
	if (!battles.contains(s)) {
	    addSite(s);
	    border.add(s);
	    totalSites++;
	    s.set(Site.State.BORDER);
	}
    }
    
    public void addBattle(Site s) {
	if (border.contains(s)) {
	    border.remove(s);
	    battles.add(s);
	    s.remove(Site.State.BORDER);
	} else {
	    addSite(s);
	    battles.add(s);
	    totalSites++;
	}
	s.set(Site.State.BATTLE);
    }

    public void addInterior(Site s) {
	addSite(s);
	interior.add(s);
	totalSites++;
	s.set(Site.State.INTERIOR);
    }

    public void reset() {
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
