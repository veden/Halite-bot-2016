import java.util.ArrayList;

public class Networking {
    public static final int SIZE_OF_INTEGER_PREFIX = 4;
    public static final int CHAR_SIZE = 1;
    private static int _width, _height;
    private static ArrayList< ArrayList<Byte> > _productions;
    private static GameMap gameMap;

    static void deserializeGameMapSize(String inputString) {
        String[] inputStringComponents = inputString.split(" ");

        _width = Integer.parseInt(inputStringComponents[0]);
        _height = Integer.parseInt(inputStringComponents[1]);
    }


    static void deserializeProductions(String inputString) {
        String[] inputStringComponents = inputString.split(" ");

        int index = 0;
        _productions = new ArrayList< ArrayList<Byte> >();
        for(int a = 0; a < _height; a++) {
            ArrayList<Byte> row = new ArrayList<Byte>();
            for(int b = 0; b < _width; b++) {
                row.add(Byte.parseByte(inputStringComponents[index]));
                index++;
            }
            _productions.add(row);
        }
    }

    static String serializeMoveList(ArrayList<Move> moves) {
        StringBuilder builder = new StringBuilder();
        for(Move move : moves) builder.append(move.x + " " + move.y + " " + move.dir.ordinal() + " ");
        return builder.toString();
    }

    static GameMap deserializeGameMap(String inputString) {
        String[] inputStringComponents = inputString.split(" ");

	boolean setProductions = false;
	if (gameMap == null) {
	    gameMap = new GameMap(_width, _height);
	    MyBot.ai = new AI(_width, _height);
	    MyBot.map = gameMap;
	    setProductions = true;
	}

	// Run-length encode of owners
	int y = 0, x = 0;
	int counter = 0;
	byte owner = 0;
	int currentIndex = 0;
	while(y != gameMap.height) {
	    counter = Integer.parseInt(inputStringComponents[currentIndex]);
	    owner = Byte.parseByte(inputStringComponents[currentIndex + 1]);
	    currentIndex += 2;
	    for(int a = 0; a < counter; ++a) {
	        gameMap.getSite(x, y).owner = owner;
		++x;
		if(x == gameMap.width) {
		    x = 0;
		    ++y;
		}
	    }
	}
	
	for (int a = 0; a < gameMap.height; ++a) {
	    for (int b = 0; b < gameMap.width; ++b) {
	        Short strengthInt = Short.parseShort(inputStringComponents[currentIndex]);
		currentIndex++;
		Site s = gameMap.getSite(b, a);
		s.units = strengthInt;
		if (setProductions) {
		    s.generator = _productions.get(a).get(b);
		    s.initialUnits = strengthInt;
		}
		s.reset();
	    }
	}

	if (_productions.size() > 0)
	    _productions.clear();

	return gameMap;
    }

    static void sendString(String sendString) {
        System.out.print(sendString+'\n');
        System.out.flush();
    }

    static String getString() {
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
            return null; // the java compiler is stupid
        }
    }

    static InitPackage getInit() {
        InitPackage initPackage = new InitPackage();
        initPackage.myID = (int)Integer.parseInt(getString());
        deserializeGameMapSize(getString());
        deserializeProductions(getString());
        initPackage.map = deserializeGameMap(getString());

        return initPackage;
    }

    static void sendInit(String name) {
        sendString(name);
    }

    static GameMap getFrame() {
        return deserializeGameMap(getString());
    }

    static void sendFrame(ArrayList<Move> moves) {
        sendString(serializeMoveList(moves));
    }

}
