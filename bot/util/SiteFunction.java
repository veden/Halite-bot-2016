package bot.util;

import java.util.BitSet;
import java.util.HashSet;

import game.Site;

public class SiteFunction {
    public float initialWeight = 0;
    public float initialValue = 0;

    public float total = 0;
    public float totalWeight = 1f;

    public SiteFunction setInitial(float initialValue) {
	this.initialValue = initialValue;
	total = 0;
	totalWeight = 1f;
	return this;
    }

    public void postProcess() {

    }
    
    public void process(Site s, BitSet used, HashSet<Site> current, HashSet<Site> next, Site center, float distance) {
	
    }
}
