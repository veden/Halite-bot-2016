package game.bot.model;

import java.util.ArrayList;

import game.GameMap;
import game.Site;

abstract public class Entity {
    public byte id;
    public short totalGenerator = 0;
    public short totalUnits = 0;
    public short totalSites = 0;

    public GameMap map;
    
    public ArrayList<Site> battles = new ArrayList<Site>();
    public ArrayList<Site> interior = new ArrayList<Site>();
    public ArrayList<Site> border = new ArrayList<Site>();
    public ArrayList<Site> frontier = new ArrayList<Site>(); 

    public Entity(byte id, GameMap map) {
	this.id = id;
	this.map = map;
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
	totalGenerator = 0;
	totalUnits = 0;
	totalSites = 0;
    }
}
