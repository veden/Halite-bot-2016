package bot.util;

import java.util.BitSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.function.Predicate;

import game.Site;

public class RingIterator implements Iterator<HashSet<Site>> {

    private Predicate<Site> neighborPredicate = null;
    private HashSet<Site> currentSet = new HashSet<Site>();
    private BitSet used = new BitSet();

    public RingIterator(Site center) {
        currentSet.add(center);
	used.set(center.id);
    }
    
    public RingIterator(Site center, Predicate<Site> neighborPredicate) {
	this(center);
	this.neighborPredicate = neighborPredicate;
    }

    @Override
    public boolean hasNext() {
	return !currentSet.isEmpty();
    }

    @Override
    public HashSet<Site> next() {
	HashSet<Site> ring = new HashSet<Site>();
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
