package game;

import java.util.HashSet;

import bot.AI;
import bot.util.RingIterator;

import game.Site.Direction;
import game.Site.State;

public class Networking {
    private GameMap map;

    public Networking(GameMap map) {
	this.map = map;
	map.bot = new AI(Byte.parseByte(getString()), map);
        deserializeGameMapSize(getString());
        deserializeProductions(getString());
        deserializeGameMap(getString());
    }
    
    private void deserializeGameMapSize(String inputString) {
        String[] inputStringComponents = inputString.split(" ");
	map.buildSites(Byte.parseByte(inputStringComponents[0]),
		       Byte.parseByte(inputStringComponents[1]));
    }


    private void deserializeProductions(String inputString) {
        String[] inputStringComponents = inputString.split(" ");
	
        int index = 0;
        for(int a = 0; a < map.height; a++)
            for(int b = 0; b < map.width; b++) {
		byte gen = Byte.parseByte(inputStringComponents[index++]);
		Site center = map.getSite(b, a);
                center.generator = gen;
		Stats.totalGenerator += center.generator;
		center.neighbors.put(Direction.NORTH, map.getSite(b, a - 1));
		center.neighbors.put(Direction.EAST, map.getSite(b + 1, a));
		center.neighbors.put(Direction.SOUTH, map.getSite(b, a + 1));
		center.neighbors.put(Direction.WEST,  map.getSite(b - 1, a));
		if (center.generator > Stats.maxGenerator)
		    Stats.maxGenerator = center.generator;
		if (center.generator < Stats.minGenerator)
		    Stats.minGenerator = center.generator;
		if (!Stats.siteCounter.containsKey(gen))
		    Stats.siteCounter.put(gen, 1);
		else
		    Stats.siteCounter.put(gen, Stats.siteCounter.get(gen)+1);
	    }
	Site.MAX_STRENGTH_LOSSY = Site.MAX_STRENGTH + Stats.maxGenerator; 
	for (int i = 0; i < map.sites.length; i++) {
	    Site s = map.sites[i];
	    RingIterator ri = new RingIterator(s);
	    float total = s.generator;
	    for (int d = 0; d < 6 && ri.hasNext(); d++) {
		HashSet<Site> ring = ri.next();
		for (Site r : ring) 
		    total += r.generator;
	    }
	    s.sitePotential = total / Stats.totalGenerator;
	    if (s.sitePotential > Stats.maxSitePotential)
		Stats.maxSitePotential = s.sitePotential;
	}
    }

    private void deserializeGameMap(String inputString) {
	String[] inputStringComponents = inputString.split(" ");

	// Run-length encode of owners
	int y = 0;
	int x = 0;
	int counter = 0;
	byte owner = 0;
	int currentIndex = 0;
	while(y != map.height) {
	    counter = Integer.parseInt(inputStringComponents[currentIndex]);
	    owner = Byte.parseByte(inputStringComponents[currentIndex + 1]);
	    currentIndex += 2;
	    for(int a = 0; a < counter; ++a) {
		Site s = map.getSite(x, y);
		s.reset();
		s.owner = owner;
		if (s.owner == 0)
		    s.set(State.NEUTRAL);
		else if (s.owner == map.bot.id)
		    s.set(State.MINE);
		else
		    s.set(State.ENEMY);
		++x;
		if(x == map.width) {
		    x = 0;
		    ++y;
		}
	    }
	}
	
	for (int a = 0; a < map.height; ++a)
	    for (int b = 0; b < map.width; ++b) {
		Short strengthInt = Short.parseShort(inputStringComponents[currentIndex]);
		currentIndex++;
		Site s = map.getSite(b, a);
		s.units = strengthInt; 
	    }
	for (int i = 0; i < Stats.totalSites; i++)
	    map.classifySite(map.sites[i]);
    }

    private void sendString(String sendString) {
	System.out.print(sendString+'\n');
	System.out.flush();
    }

    private String getString() {
	try {
	    StringBuilder builder = new StringBuilder();
	    int buffer;
	    while ((buffer = System.in.read()) >= 0) {
		if (buffer == '\n') {
		    break;
		} else {
		    builder = builder.append((char)buffer);
		}
	    }
	    if(builder.charAt(builder.length()-1) == '\r') builder.setLength(builder.length()-1); //Removes a carriage return if on windows for manual testing.
	    return builder.toString();
	} catch(Exception e) {
	    System.exit(1);
	}
	return null;
    }

    public void sendInit(String name) {
	sendString(name);
    }

    public void getFrame() {
	deserializeGameMap(getString());
    }

    public void sendFrame() {
	StringBuilder sb = new StringBuilder();
	for (int i = 0; i < map.sites.length; i++) {
	    Site s = map.sites[i];
	    if (s.heading != Direction.STILL)
		sb.append(s.encodeMove());
	}
	sendString(sb.toString());
    }
}
