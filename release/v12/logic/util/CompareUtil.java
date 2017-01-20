package logic.util;

import java.util.Comparator;

import game.Site;
import game.Site.P;

public class CompareUtil {

    public static Comparator<Site> maxProperty(P siteProperty) {
	return new Comparator<Site>() {
	    @Override
	    public int compare(Site o1, Site o2) {
		float v = o2.value(siteProperty) - o1.value(siteProperty);
		if (v == 0)
		    return o1.id - o2.id;
		return v > 0 ? 1 : -1;
	    }
	};
    }
}
