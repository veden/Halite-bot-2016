package game;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;

import game.bot.Bot;
import game.bot.model.Player;

public class GameMap{
    private Site[] sites;
    public int width;
    public int height;
    public int totalSites;
    public byte maxGenerator = 0;
    public byte minGenerator = 100;
    public short maxUnit = 0;
    public short minUnit = 255;

    public Bot bot;

    public HashMap<Byte, Player> enemies = new HashMap<Byte, Player>();
    public HashSet<Site> battles = new HashSet<Site>();
    
    public void reset() {
	battles.clear();
	bot.reset();
	for (Entry<Byte, Player> e : enemies.entrySet())
	    e.getValue().reset();
	maxGenerator = 0;
	minGenerator = 100;
	maxUnit = 0;
	minUnit = (short)255;
    }

    public void play() {
	bot.move();
    }
    
    public void buildSites(byte width, byte height) {
        this.width = width;
        this.height = height;
	this.totalSites = width * height;
        sites = new Site[width * height];
        for(byte x = 0; x < width; x++)
            for(byte y = 0; y < height; y++)
		sites[y + (height * x)] = new Site(x, y);
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
