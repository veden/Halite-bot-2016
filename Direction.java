import java.util.Random;

public enum Direction {
    STILL, NORTH, EAST, SOUTH, WEST;
    public static final Direction[] DIRECTIONS = new Direction[]{STILL, NORTH, EAST, SOUTH, WEST};
    public static final Direction[] CARDINALS = new Direction[]{NORTH, EAST, SOUTH, WEST};

    public static Direction toDirection(int value) {
	if(value == 0) {
	    return STILL;
	}
	if(value == 1) {
	    return NORTH;
	}
	if(value == 2) {
	    return EAST;
	}
	if(value == 3) {
	    return SOUTH;
	}
	if(value == 4) {
	    return WEST;
	}
	return null;
    }

    public static Direction randomDirection() {
        return toDirection(new Random().nextInt(5));
    }

    public static Direction toInverseDirection(int i) {
	if (i == 0)
	    return Direction.STILL;
	else if (i == 1)
	    return Direction.SOUTH;
	else if (i == 2)
	    return Direction.WEST;
	else if (i == 3)
	    return Direction.NORTH;
	else if (i == 4)
	    return Direction.EAST;
	return null;
    }
}
