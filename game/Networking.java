package game;

import game.bot.Bot;
import game.bot.model.Player;

public class Networking {
    public Networking() {
        Harness.map.bot = new Bot(Byte.parseByte(getString()));
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
                center.generator = gen;
		center.neighbors[0] = Harness.map.getSite(b, a - 1);
		center.neighbors[1] = Harness.map.getSite(b + 1, a);
		center.neighbors[2] = Harness.map.getSite(b, a + 1);
		center.neighbors[3] = Harness.map.getSite(b - 1, a);
		center.distances = new byte[Harness.map.totalSites];
		for (int i = 0; i < Harness.map.totalSites; i++)
		    center.distances[i] = (byte)Harness.map.manhattanDistance(center, Harness.map.getSite(i));
		if (gen > Harness.map.maxGenerator)
		    Harness.map.maxGenerator = gen;
		if (gen < Harness.map.minGenerator)
		    Harness.map.minGenerator = gen;
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
	        s.owner = owner;
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
		s.reset();
		if (s.units > Harness.map.maxUnit)
		    Harness.map.maxUnit = s.units;
		if (s.units < Harness.map.minUnit)
		    Harness.map.minUnit = s.units;
	    }
    }

    private void analyzeSite(Site s) {
	if (s.mine()) {
	    boolean border = false;
	    for (int i = 0; i < Direction.CARDINALS.length; i++)
		if (s.neighbors[i].neutral())
		    border = true;
	    if (!border)
		Harness.map.bot.addInterior(s);
	    else
		Harness.map.bot.addBorder(s);
	} else if (s.neutral()) {
	    if (s.units == 0) {
		Harness.map.battles.add(s);
		for (int i = 0; i < Direction.CARDINALS.length; i++) {
		    Site neighbor = s.neighbors[i];
		    if (neighbor.mine())
			Harness.map.bot.borderToContact(neighbor); //TODO check contact flag
		    else if (neighbor.enemy()) {
			if (!Harness.map.enemies.containsKey(s.owner))
			    Harness.map.enemies.put(s.owner, new Player((byte)s.owner));
			Harness.map.enemies.get(s.owner).borderToContact(s);
		    }
		}		
	    } else {
		boolean frontier = false;
		boolean frontierEnemy = false;
		for (int i = 0; i < Direction.CARDINALS.length; i++) {
		    Site neighbor = s.neighbors[i];
		    if (neighbor.mine())
			frontier = true;
		    else if (neighbor.enemy())
			frontierEnemy = true;
		}
		if (!frontier)
		    Harness.map.bot.addUnexplored(s);
		else
		    Harness.map.bot.addFrontier(s);
		if (frontierEnemy) {
		    if (!Harness.map.enemies.containsKey(s.owner))
			Harness.map.enemies.put(s.owner, new Player((byte)s.owner));
		    Harness.map.enemies.get(s.owner).addFrontier(s);
		}
		    
	    }
	} else if (s.enemy()) {
	    boolean border = false;
	    for (int i = 0; i < Direction.CARDINALS.length; i++)
		if (s.neighbors[i].neutral())
		    border = true;
	    if (!Harness.map.enemies.containsKey(s.owner))
		Harness.map.enemies.put(s.owner, new Player((byte)s.owner));
	    Player enemy = Harness.map.enemies.get(s.owner);
	    if (!border)
		enemy.addInterior(s);
	    else
		enemy.addBorder(s);
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
