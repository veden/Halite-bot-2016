public class PreComputed {

    public float[] totalUnitSet;
    public float[] reductionSet;
    public float[] damageSet;
    
    public PreComputed(int maxRadius) {
	
	totalUnitSet = new float[maxRadius+1];
	for (int i = 0; i <= maxRadius; i++)
	    totalUnitSet[i] = (i+1)*Constants.SITE_MAX_STRENGTH;

	reductionSet = new float[maxRadius+5];
	for (int i = 0; i <= maxRadius + 4; i++)
	    reductionSet[i] = 1f / (i+1);

	damageSet = new float[(int)Constants.SITE_MAX_STRENGTH+1];
	for (int i = 0; i <= Constants.SITE_MAX_STRENGTH; i++)
	    damageSet[i] = i / Constants.SITE_MAX_STRENGTH;
    }
}
