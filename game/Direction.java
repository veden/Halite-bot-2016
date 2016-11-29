package game;

public enum Direction {
    NORTH, EAST, SOUTH, WEST, STILL;
    public static final Direction[] DIRECTIONS = new Direction[]{STILL, NORTH, EAST, SOUTH, WEST};
    public static final Direction[] CARDINALS = new Direction[]{NORTH, EAST, SOUTH, WEST};

    public static int encodeDirection(Direction d) {
	int o;
	if (d == Direction.NORTH)
	    o = 1;
	else if (d == Direction.EAST)
	    o = 2;
	else if (d == Direction.SOUTH)
	    o = 3;
	else if (d == Direction.WEST)
	    o = 4;
	else
	    o = 0;
	return o;
    }
    
    // public static Direction toDirection(int value) {
    // 	if(value == 0)
    // 	    return STILL;
    // 	if(value == 1)
    // 	    return NORTH;
    // 	if(value == 2)
    // 	    return EAST;
    // 	if(value == 3)
    // 	    return SOUTH;
    // 	if(value == 4)
    // 	    return WEST;
    // 	return null;
    // }

    // public static Direction randomDirection() {
    //     return toDirection(new Random().nextInt(5));
    // }

    // public static Direction toInverseDirection(int i) {
    // 	if (i == 0)
    // 	    return Direction.STILL;
    // 	else if (i == 1)
    // 	    return Direction.SOUTH;
    // 	else if (i == 2)
    // 	    return Direction.WEST;
    // 	else if (i == 3)
    // 	    return Direction.NORTH;
    // 	else if (i == 4)
    // 	    return Direction.EAST;
    // 	return null;
    // }
}
