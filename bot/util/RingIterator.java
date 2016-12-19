package bot.util;

import java.util.HashSet;
import java.util.Iterator;
import java.util.function.Predicate;

import game.Site;

public class RingIterator implements Iterator<HashSet<Site>> {

    private Predicate<Site> neighborPredicate = null;
    private HashSet<Site> currentSet = new HashSet<Site>();

    public RingIterator(Site center) {
        currentSet.add(center);	
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
		    if (!currentSet.contains(neighbor) && neighborPredicate.test(neighbor))
			ring.add(neighbor);
	} else {
	    for (Site s : currentSet)
		for (Site neighbor : s.neighbors.values())
		    if (!currentSet.contains(neighbor))
			ring.add(neighbor);
	}
	currentSet = ring;
	return ring;
    }
}
