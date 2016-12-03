package game;

import game.bot.Bot;
import game.bot.model.Enemy;

public class Networking {
    public Networking() {
        Harness.bot = new Bot(Byte.parseByte(getString()), Harness.map);
        deserializeGameMapSize(getString());
        deserializeProductions(getString());
        deserializeGameMap(getString());
    }
    
    private void deserializeGameMapSize(String inputString) {
        String[] inputStringComponents = inputString.split(" ");
	Harness.map.buildSites(Byte.parseByte(inputStringComponents[0]),
			       Byte.parseByte(inputStringComponents[1]));
    }


    private void deserializeProductions(String inputString) {
        String[] inputStringComponents = inputString.split(" ");
	
        int index = 0;
        for(int a = 0; a < Harness.map.height; a++)
            for(int b = 0; b < Harness.map.width; b++) {
		byte gen = Byte.parseByte(inputStringComponents[index++]);
		Site center = Harness.map.getSite(b, a);
                center.setGenerator(gen);
		Harness.map.totalGenerator += center.generator;
		center.neighbors.put(Direction.NORTH, Harness.map.getSite(b, a - 1));
		center.neighbors.put(Direction.EAST, Harness.map.getSite(b + 1, a));
		center.neighbors.put(Direction.SOUTH, Harness.map.getSite(b, a + 1));
		center.neighbors.put(Direction.WEST,  Harness.map.getSite(b - 1, a));
		if (center.generator > Harness.map.maxGenerator)
		    Harness.map.maxGenerator = center.generator;
		if (center.generator < Harness.map.minGenerator)
		    Harness.map.minGenerator = center.generator;
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
	while(y != Harness.map.height) {
	    counter = Integer.parseInt(inputStringComponents[currentIndex]);
	    owner = Byte.parseByte(inputStringComponents[currentIndex + 1]);
	    currentIndex += 2;
	    for(int a = 0; a < counter; ++a) {
		Site s = Harness.map.getSite(x, y);
		s.reset();
		s.owner = owner;
		if (owner == 0)
		    s.set(Site.State.NEUTRAL);
		else if (owner == Harness.bot.id)
		    s.set(Site.State.MINE);
		else
		    s.set(Site.State.ENEMY);
		++x;
		if(x == Harness.map.width) {
		    x = 0;
		    ++y;
		}
	    }
	}
	
	for (int a = 0; a < Harness.map.height; ++a)
	    for (int b = 0; b < Harness.map.width; ++b) {
		Short strengthInt = Short.parseShort(inputStringComponents[currentIndex]);
		currentIndex++;
		Site s = Harness.map.getSite(b, a);
		s.units = strengthInt;
		analyzeSite(s);
	    }
    }

    //not all sites will have strength at this point
    private void analyzeSite(Site site) {
	if (site.get(Site.State.MINE) || site.get(Site.State.ENEMY)) {
	    if (site.get(Site.State.MINE) && (site.units == 0))
		site.set(Site.State.USED);
	    
	    boolean border = false;
	    for (Site neighbor : site.neighbors.values())
		if (neighbor.get(Site.State.NEUTRAL)) {
		    border = true;
		    break;
		}

	    if (site.get(Site.State.MINE)) {
		if (site.aboveActionThreshold())
		    site.set(Site.State.READY);
		if (!border)
		    Harness.bot.addInterior(site);
		else
		    Harness.bot.addBorder(site);
	    } else {
		if (!border)
		    Enemy.get(site.owner).addInterior(site);
		else
		    Enemy.get(site.owner).addBorder(site);
	    }
	} else if (site.get(Site.State.NEUTRAL)) {
	    if (site.units == 0) {
		boolean field = false;
		boolean enemy = false;
	    	for (Site neighbor : site.neighbors.values())
	    	    if (neighbor.get(Site.State.MINE)) {
	    		Harness.bot.addBattle(neighbor);
			field = true;
		    } else if (neighbor.get(Site.State.ENEMY)) {
	    		Enemy.get(neighbor.owner).addBattle(neighbor);
			enemy = true;
		    }
		if (field && !enemy)
		    Harness.map.addField(site);
		Harness.map.addBattle(site);
	    } else {
	    	boolean frontier = false;
	    	boolean frontierEnemy = false;
	    	for (Site neighbor : site.neighbors.values()) {
	    	    if (neighbor.get(Site.State.MINE))
	    		frontier = true;
	    	    else if (neighbor.get(Site.State.ENEMY))
	    		frontierEnemy = true;
	    	}
		if (frontier && frontierEnemy) {
		    Harness.bot.addBattle(site);
		    Enemy.get(site.owner).addBattle(site);
		    Harness.map.addBattle(site);
		} else {
		    if (frontier)
			Harness.bot.addFrontier(site);
		    if (frontierEnemy)
			Enemy.get(site.owner).addFrontier(site);
		    Harness.map.addUnexplored(site);
		}
	    }
	}
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
	for (int i = 0; i < Harness.map.totalSites; i++) {
	    Site s = Harness.map.getSite(i);
	    if (s.heading != Direction.STILL)
		sb.append(s.encodeMove());
	}
	sendString(sb.toString());
    }
}
