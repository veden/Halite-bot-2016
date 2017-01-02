package logic.util;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Iterator;
import java.util.function.Predicate;

import game.Site;

public class RingIterator implements Iterator<ArrayList<Site>> {

    private Predicate<Site> neighborPredicate = null;
    private ArrayList<Site> currentSet = new ArrayList<Site>();
    private BitSet used = new BitSet();

    public RingIterator(Site center) {
        addCenter(center);
    }

    public RingIterator(ArrayList<Site> centers) {
	for (Site s : centers)
	    addCenter(s);
    }

    public RingIterator(Site center, Predicate<Site> neighborPredicate) {
	this(center);
	this.neighborPredicate = neighborPredicate;
    }

    public RingIterator(ArrayList<Site> centers, Predicate<Site> neighborPredicate) {
	this(centers);
	this.neighborPredicate = neighborPredicate;
    }

    public void addCenter(Site center) {
	currentSet.add(center);
	used.set(center.id);
    } 

    @Override
    public boolean hasNext() {
	return !currentSet.isEmpty();
    }

    @Override
    public ArrayList<Site> next() {
        ArrayList<Site> ring = new ArrayList<Site>();
	boolean usePredicate = neighborPredicate != null;
	if (usePredicate) {
	    for (Site s : currentSet)
		for (Site neighbor : s.neighbors.values())
		    if (!used.get(neighbor.id) && neighborPredicate.test(neighbor)) {
			ring.add(neighbor);
			used.set(neighbor.id);
		    }
	} else {
	    for (Site s : currentSet)
		for (Site neighbor : s.neighbors.values())
		    if (!used.get(neighbor.id)) {
			ring.add(neighbor);
			used.set(neighbor.id);
		    }
	}
	currentSet = ring;
	return ring;
    }
}
