package bot.model;

import java.util.ArrayList;
import java.util.function.Predicate;

import bot.util.RingIterator;

import game.Site;

public class Objective {

    private ArrayList<Site> primary = new ArrayList<Site>();
    private Site center;
    private float score;

    private Predicate<Site> p;
    
    public Objective(Site center, Predicate<Site> p) {
	this.center = center;
	this.p = p;
	RingIterator ri = new RingIterator(center, p);
	for (Site s : ri.next()) {
	    
	}
    }
}
