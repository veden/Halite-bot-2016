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
    
    public ArrayList<Site> battles = new ArrayList<Site>();
    public ArrayList<Site> interior = new ArrayList<Site>();
    public ArrayList<Site> border = new ArrayList<Site>();
    public ArrayList<Site> frontier = new ArrayList<Site>();
    public ArrayList<Site> spear = new ArrayList<Site>();
    public ArrayList<Site> fields = new ArrayList<Site>();
    
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

    public void addField(Site s) {
        fields.add(s);
	s.set(State.FIELD);
    }

    public void addSpear(Site s) {
	if (!border.contains(s)) {
	    addSite(s);
	} else {
	    border.remove(s);
	    s.remove(State.BORDER);
	    s.set(State.SPEAR);
	}
	spear.add(s);
    }
    
    public void addFrontier(Site s) {
	frontier.add(s);
	s.set(State.FRONTIER);
    }

    public void addBorder(Site s) {
	if (!battles.contains(s)) {
	    addSite(s);
	    border.add(s);
	    totalSites++;
	    s.set(State.BORDER);
	}
    }
    
    public void addBattle(Site s) {
	if (border.contains(s)) {
	    border.remove(s);
	    battles.add(s);
	    s.remove(State.BORDER);
	} else {
	    addSite(s);
	    battles.add(s);
	    totalSites++;
	}
	s.set(State.BATTLE);
    }

    public void addInterior(Site s) {
	addSite(s);
	interior.add(s);
	totalSites++;
	s.set(State.INTERIOR);
    }

    public void reset() {
	fields.clear();
	battles.clear();
	interior.clear();
	border.clear();
	frontier.clear();
	spear.clear();
	totalPotentialGeneration += totalGenerator;
	totalGenerator = 0;
	totalUnits = 0;
	totalSites = 0;
    }
}
