package logic.world;

import game.Site;

import logic.Constants;

public class Tile implements Comparable<Tile> {

    public static int counter = 1;
    public static float maxProduction = -1.0f;
    
    private Site site;
    public float production;
    public float strength;
    public float defense;
    public int x;
    public int y;
    public int openings = -1;
    public int ambushes = -1;
    public boolean used;
    public final int id;
    
    public Tile(int x, int y, Site site) {
	this.id = counter++;
	this.site = site;
	this.x = x;
	this.y = y;
	used = false;
	if (site.production > maxProduction)
	    maxProduction = site.production;
    }

    public static float getProductionScaling() {
	if (maxProduction != -1)
	    return (Constants.SITE_MAX_STRENGTH / maxProduction) * Constants.GAME_SCALING;
	return maxProduction;
    }

    public int getOwner() {
	return site.owner;
    }

    public boolean aboveStrengthThreshold() {
	return getProductionGenerator() * Constants.SITE_ACCUMULATION_MULTIPLIER < getStrengthGenerator();
    }
    
    public int getProductionGenerator() {
	return site.production;
    }

    public int getStrengthGenerator() {
	return site.strength;
    }

    public void updateTile(Site site) {
	this.site = site;
	used = false;
    } 
    
    public String toString() {
	return "x-" + x + " y-" + y + " gStr-" + site.strength + " gProd-" + site.production + " o-" + site.owner + " prod-" + production + " str-" + strength + " def-" + defense + " open-" + openings + " amb-" + ambushes;
    }

    @Override
    public int compareTo(Tile t) {
	float thatT = (t.production - t.defense);
	float thisT = (this.production - this.defense);
	float v = (thatT-t.strength) - (thisT-this.strength);
	if (v == 0) {
	    if (v == 0) {
		//		v = t.ambushes - this.ambushes;
		if (v == 0) {
		    //  v = t.openings - this.openings;
		    if (v == 0)
			return this.id - t.id;
		}
	    }
	}
	return v>0?1:-1;
    }
}
