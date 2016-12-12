package game;

import game.bot.AI;

public class Networking {
    public Networking(AI ai) {
        ai.id = Byte.parseByte(getString());
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
		Stats.totalGenerator += center.generator;
		center.neighbors.put(Direction.NORTH, Harness.map.getSite(b, a - 1));
		center.neighbors.put(Direction.EAST, Harness.map.getSite(b + 1, a));
		center.neighbors.put(Direction.SOUTH, Harness.map.getSite(b, a + 1));
		center.neighbors.put(Direction.WEST,  Harness.map.getSite(b - 1, a));
		if (center.generator > Stats.maxGenerator)
		    Stats.maxGenerator = center.generator;
		if (center.generator < Stats.minGenerator)
		    Stats.minGenerator = center.generator;
		if (!Stats.siteCounter.containsKey(gen))
		    Stats.siteCounter.put(gen, 1);
		else
		    Stats.siteCounter.put(gen, Stats.siteCounter.get(gen)+1);
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
		s.newOwner = owner;
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
		s.newUnits = strengthInt;
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
