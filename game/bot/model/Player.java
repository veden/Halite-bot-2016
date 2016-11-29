package game.bot.model;

import java.util.HashSet;

import game.Site;

public class Player {
    public byte id;
    public short totalGenerator;
    public short totalUnits;
    public short totalSites;
    public HashSet<Site> contacts = new HashSet<Site>();
    public HashSet<Site> interior = new HashSet<Site>();
    public HashSet<Site> border = new HashSet<Site>();
    public HashSet<Site> frontier = new HashSet<Site>();
    
    public Player(byte id) {
	this.id = id;
    }

    public void addInterior(Site s) {
	interior.add(s);
	addSite(s);
    }

    public void addFrontier(Site s) {
        frontier.add(s);
    }

    public void addContact(Site s) {
	contacts.add(s);
	addSite(s);
    }

    public void borderToContact(Site s) {
	border.remove(s);
	contacts.add(s);
    }

    public void addBorder(Site s) {
	border.add(s);
	addSite(s);
    }

    private void addSite(Site s) {
        totalGenerator += s.generator;
	totalUnits += s.units;
	totalSites++;	
    }

    public void move() {}

    public void reset() {
	interior.clear();
	contacts.clear();
	border.clear();
	frontier.clear();
	totalSites = 0;
	totalGenerator = 0;
	totalUnits = 0;
    }
}
