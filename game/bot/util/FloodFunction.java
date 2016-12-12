package game.bot.util;

import java.util.BitSet;
import java.util.HashSet;

import game.Site;

public class FloodFunction {

    public float best = 0;
    public boolean abort = false;

    public FloodFunction setInitial(float best) {
	this.best = best;
	this.abort = false;
	return this;
    }

    public void scan(Site neighbor, Site center) {
	
    }

    public void process(Site s, BitSet used, Site center, float distance, HashSet<Site> current, HashSet<Site> next) {
	
    }
}
