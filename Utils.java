public class Utils {

    public static short safeCoordinate(int x, int limit) {
	if (x < 0) 
	    return (short)(limit + (x % limit));
	else
	    return (short)(x % limit);
    }

    public static short coordinateToIndex(int x, int y) {
	return (short)(y + (MyBot.map.height * x));
    }
}
