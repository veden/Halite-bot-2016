package game;

import logic.AI;
import logic.Constants.D;
import logic.Constants.P;
import logic.Constants.S;

public class Networking {
    private GameMap map;

    public Networking(GameMap map) {
	this.map = map;
	map.bot = new AI(Integer.parseInt(getString()), map);
        deserializeGameMapSize(getString());
        deserializeProductions(getString());
        deserializeGameMap(getString());
    }
    
    private void deserializeGameMapSize(String inputString) {
        String[] inputStringComponents = inputString.split(" ");
	map.buildSites(Integer.parseInt(inputStringComponents[0]),
		       Integer.parseInt(inputStringComponents[1]));
    }


    private void deserializeProductions(String inputString) {
        String[] inputStringComponents = inputString.split(" ");
	
        int index = 0;
        for(int a = 0; a < map.height; a++)
            for(int b = 0; b < map.width; b++) {
	        float gen = Float.parseFloat(inputStringComponents[index++]);
		Site center = map.getSite(b, a);
		center.set(P.GENERATOR, (float)gen);
		Stats.totalGenerator += center.v(P.GENERATOR);
		center.neighbors.put(D.NORTH, map.getSite(b, a - 1));
		center.neighbors.put(D.EAST, map.getSite(b + 1, a));
		center.neighbors.put(D.SOUTH, map.getSite(b, a + 1));
		center.neighbors.put(D.WEST,  map.getSite(b - 1, a));
		if (center.v(P.GENERATOR) > Stats.maxGenerator)
		    Stats.maxGenerator = center.v(P.GENERATOR);
		if (center.v(P.GENERATOR) < Stats.minGenerator)
		    Stats.minGenerator = center.v(P.GENERATOR);
		if (!Stats.siteCounter.containsKey(gen))
		    Stats.siteCounter.put((float)gen, 1.0f);
		else
		    Stats.siteCounter.put((float)gen, Stats.siteCounter.get(gen)+1f);
	    }
    }

    private void deserializeGameMap(String inputString) {
	String[] inputStringComponents = inputString.split(" ");

	// Run-length encode of owners
	int y = 0;
	int x = 0;
	int counter = 0;
        int owner = 0;
	int currentIndex = 0;
	while(y != map.height) {
	    counter = Integer.parseInt(inputStringComponents[currentIndex]);
	    owner = Integer.parseInt(inputStringComponents[currentIndex + 1]);
	    currentIndex += 2;
	    for(int a = 0; a < counter; ++a) {
		Site s = map.getSite(x, y);
		s.assign(owner, s.owner, map.bot.id, s.is(S.OBJECTIVE), map.scaling);
		++x;
		if(x == map.width) {
		    x = 0;
		    ++y;
		}
	    }
	}
	
	for (int a = 0; a < map.height; ++a)
	    for (int b = 0; b < map.width; ++b) {
	        int strengthInt = Integer.parseInt(inputStringComponents[currentIndex]);
		currentIndex++;
		Site s = map.getSite(b, a);
		s.units = strengthInt;
		map.classifySite(s);
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
	for (int i = 0; i < map.sites.length; i++) {
	    Site s = map.sites[i];
	    if (s.heading != D.STILL)
		sb.append(s.encodeMove());
	}
	sendString(sb.toString());
    }
}
