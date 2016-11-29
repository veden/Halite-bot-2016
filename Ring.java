import gnu.trove.list.array.TShortArrayList;
import gnu.trove.map.hash.TShortFloatHashMap;

public class Ring {
    public TShortArrayList[] layers;
    public TShortFloatHashMap totals;

    public Ring(TShortArrayList[] layers, TShortFloatHashMap totals) {
	this.layers = layers;
	this.totals = totals;
    }
}
