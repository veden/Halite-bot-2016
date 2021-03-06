package logic.util;

import java.util.Comparator;

import game.Site;

import logic.Constants.F;
import logic.Constants.P;

public class CompareUtil {

    public static Comparator<Site> maxProperty(P siteProperty) {
	return new Comparator<Site>() {
	    @Override
	    public int compare(Site o1, Site o2) {
		float v = o2.v(siteProperty) - o1.v(siteProperty);
		if (v == 0)
		    return o1.id - o2.id;
		return v > 0 ? 1 : -1;
	    }
	};
    }


    public static Comparator<Site> maxField(F field) {
	return new Comparator<Site>() {
	    @Override
	    public int compare(Site o1, Site o2) {
		float v = o2.v(field) - o1.v(field);
		if (v == 0)
		    return o1.id - o2.id;
		return v > 0 ? 1 : -1;
	    }
	};
    }
    
    public static Comparator<Site> minProperty(P siteProperty) {
	return new Comparator<Site>() {
	    @Override
	    public int compare(Site o1, Site o2) {
		float v = o1.v(siteProperty) - o2.v(siteProperty);
		if (v == 0)
		    return o1.id - o2.id;
		return v > 0 ? 1 : -1;
	    }
	};
    }
}
