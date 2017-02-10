package logic.util;

public class MathUtil {

    public static float normalize(float x, float low, float high) {
	if (low == high)
	    return 1f;
	if (x < low)
	    return 0;
	if (x > high)
	    return 1f;
	return ((x - low) / (high - low));
    }
}
