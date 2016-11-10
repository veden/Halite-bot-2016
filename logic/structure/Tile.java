package logic.structure;

import java.util.HashMap;

import game.MyBot;
import game.Site;

import logic.constant.Constants;
import logic.constant.Layer;
import logic.unit.Worm;

public class Tile {

    public static float maxProduction = -1.0f;
    
    private Site site;
    public float production;
    public float strength;
    public float defense;
    private HashMap<Worm, HashMap<Layer, Float>> wormProperties;
    public int x;
    public int y;
    
    public Tile(int x, int y, Site site) {
	this.site = site;
	this.x = x;
	this.y = y;
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

    public int getProductionGenerator() {
	return site.production;
    }

    public int getStrengthGenerator() {
	return site.strength;
    }

    public void updateTile(Site site) {
	this.site = site;
    } 
    
    public String toString() {
	return "x-" + x + " y-" + y + " genStrength-" + site.strength + " genProduction-" + site.production + " owner-" + site.owner + " production-" + production + " strength-" + strength + " defense-" + defense;
    }
}
