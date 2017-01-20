package logic.util;

public class MathUtil {

    public static float normalize(float x, float low, float high) {
	if (low == high)
	    return 1;
	return ((x - low) / (high - low));
    }
}
