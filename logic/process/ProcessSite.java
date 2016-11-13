package logic.process;

import game.Site;

import logic.world.Map;

public class ProcessSite {
    public void process(Map map, int x, int y, Site s) {	
	map.updateTile(x, y, s);
    }
}
