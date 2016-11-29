package game.bot;

import game.Site;

public class MoveUtils {

    public static void moveSiteToSite(Site a, Site b) {
	a.outgoing += a.units;
	b.incoming += a.units;
	a.setUsed();
    }

    public static boolean validMove(Site a, Site b) {
	return (a.units + b.incoming + b.units - b.outgoing) < Site.MAX_STRENGTH_LOSSY;
    }

    public static boolean validAttack(Site a, Site b) {
	return (((a.units + b.incoming - b.units) > 0) &&
		((a.units + b.incoming - b.units) < Site.MAX_STRENGTH_LOSSY));
    }
}
