package logic.structure;

import java.util.HashMap;

import game.Site;

import logic.unit.Worm;

public class Tile {

    private Site site;
    private float prodScore;
    private float strengthScore;
    private HashMap<Worm, HashMap<Enum, Float>> wormProperties;
    private int x;
    private int y;
    
    public Tile(int x, int y, Site site) {
	this.site = site;
	this.x = x;
	this.y = y;
    }

    public void updateTile(Site site) {
	this.site = site;
    }
    
    public String toString() {
	return "x-" + x + " y-" + y + " strength-" + site.strength + " production-" + site.production + " owner-" + site.owner;
    }
}
