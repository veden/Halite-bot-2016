package logic;

import java.util.EnumSet;

public class Constants {

    //State
    public static enum S {
	BATTLE, FRONTIER, UNEXPLORED, INTERIOR, BORDER, OPEN,
	READY, OBJECTIVE, COMBAT_READY, GATE, ATTACK, USED,

	MINE, NEUTRAL, ENEMY
    }

    //Property
    public static enum P {
	EXPLORE_VALUE, GENERATOR, AGE, ACCUMULATOR, ALLOWED_UNITS, DISTANCE
    }

    //Fields
    public static enum F {
	EXPLORE, REINFORCE, DAMAGE
    }

    //Direction
    public static enum D {
	NORTH, EAST, SOUTH, WEST, STILL;
    }

    public static final EnumSet<D> DIRECTIONS = EnumSet.of(D.STILL, D.NORTH, D.EAST, D.SOUTH, D.WEST);
    public static final EnumSet<D> CARDINALS = EnumSet.of(D.NORTH, D.EAST, D.SOUTH, D.WEST);

    public static final float MAX_UNITS = 255f;
    
}
