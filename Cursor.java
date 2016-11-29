public class Cursor {

    public Site head;

    private Site highest;
    
    public Cursor(Site head) {
	this.head = head;
    }

    public boolean refresh() {
	highest = null;
        if (!MyBot.ai.isNeutral(head.x, head.y)) {
	    for (Site s : MyBot.map.getFrontSites()) {
		if (MyBot.map.manhattanDistance(head, s) <= 3) {
		    if (MyBot.ai.isNeutral(s.x, s.y) && ((highest == null) ||
							 (highest.explore < s.explore)))
			highest = s;
		}
	    }
	    if (highest != null) {
		head = highest;
		return true;
	    }
	}
	return false;
    }
}
