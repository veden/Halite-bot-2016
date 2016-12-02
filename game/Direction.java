package game;

import java.util.EnumSet;

public enum Direction {
    NORTH, EAST, SOUTH, WEST, STILL;
    public static final EnumSet<Direction> DIRECTIONS = EnumSet.of(STILL, NORTH, EAST, SOUTH, WEST);
    public static final EnumSet<Direction> CARDINALS = EnumSet.of(NORTH, EAST, SOUTH, WEST);

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

    public static Direction reverse(Direction d) {
    	if (d == Direction.NORTH)
    	    return Direction.SOUTH;
    	else if (d == Direction.EAST)
    	    return Direction.WEST;
    	else if (d == Direction.SOUTH)
    	    return Direction.NORTH;
    	else if (d == Direction.WEST)
    	    return Direction.EAST;
    	return Direction.STILL;
    }
}
