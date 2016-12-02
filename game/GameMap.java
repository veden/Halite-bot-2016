package game;

import java.util.ArrayList;
import java.util.HashSet;

public class GameMap{
    private Site[] sites;
    public float defenseRange;
    public float enemyDistance;
    public float frontierRange;
    public int width;
    public int height;
    public int totalSites;
    public short totalProduction = 0;
    public short totalUnexploredGenerator = 0;
    public byte maxGenerator = 0;
    public byte minGenerator = 100;

    public static final byte MAX_SIZE = 50;
    
    public HashSet<Site> unexplored = new HashSet<Site>();
    public ArrayList<Site> battles = new ArrayList<Site>();
    public HashSet<Site> fields = new HashSet<Site>();
    
    public void reset() {
	battles.clear();
	unexplored.clear();
    }

    public void buildSites(byte width, byte height) {
        this.width = width;
        this.height = height;
	this.totalSites = width * height;
	int size = Math.min(width, height) - 15;
	if (size < 0)
	    size = 0;
        float maxSize = (float)(MAX_SIZE - 15f);
	this.defenseRange = 0.25f - (0.15f * (size / maxSize)); //smaller is further
	this.enemyDistance = 0.35f - (0.06f * (size / maxSize)); //smaller is further
	this.frontierRange = 0.35f + (0.30f * (size / maxSize)); //larger is more paths
        sites = new Site[width * height];
        for(byte x = 0; x < width; x++)
            for(byte y = 0; y < height; y++)
		sites[y + (height * x)] = new Site(x, y);
    }

    public void addUnexplored(Site s) {
	unexplored.add(s);
	totalUnexploredGenerator += s.generator;
	s.set(Site.State.UNEXPLORED);
    }

    public void addBattle(Site s) {
        battles.add(s);
	s.set(Site.State.BATTLE);
    }

    public void addField(Site s) {
        fields.add(s);
	s.set(Site.State.FIELD);
    }
    
    public float manhattanDistance(Site s1, Site s2) {
        int dx = Math.abs(s1.x - s2.x);
        int dy = Math.abs(s1.y - s2.y);

        if(dx > width / 2.0) dx = width - dx;
        if(dy > height / 2.0) dy = height - dy;

        return dx + dy;
    }

    public Site getSite(int x, int y) {
	return sites[safeCoordinate(y, height) + (height * safeCoordinate(x, width))];
    }

    public Site getSite(int i) {
	return sites[i];
    }
    
    public static short safeCoordinate(int x, int limit) {
	if (x < 0) 
	    return (short)(limit + (x % limit));
	else
	    return (short)(x % limit);
    }
}
