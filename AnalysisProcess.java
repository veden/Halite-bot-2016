import java.util.Comparator;
import java.util.Iterator;
import java.util.TreeSet;

public class AnalysisProcess implements ProcessSite {

    private TreeSet<Cursor> pointers = new TreeSet<Cursor>(new Comparator<Cursor>() {
	    @Override
	    public int compare(Cursor c1, Cursor c2) {
		return c1.head.compareTo(c2.head);
	    }
	});
    
    public float maxExplore = -Float.MAX_VALUE;
    private int maxRadius = 0;

    private CountNeigbhors cn = new CountNeigbhors();
    private ExpandObjectiveProcess eop = new ExpandObjectiveProcess();

    private TreeSet<Site> objectiveCandidates = new TreeSet<Site>();
    public TreeSet<Site> rankedFrontSites = new TreeSet<Site>();
    public TreeSet<Site> rankedOwnedSites = new TreeSet<Site>(new Comparator<Site>() {
	    @Override
	    public int compare(Site arg0, Site arg1) {
		float v = arg1.units - arg0.units;
		if (v == 0)
		    return (arg0.x - arg1.x) - (arg0.y - arg1.y);
		return v > 0 ? 1 : -1;
	    }
	});
    
    public AnalysisProcess(int maxRadius) {
	this.maxRadius = maxRadius;
    }
    
    public void reset() {
	objectiveCandidates.clear();
	rankedFrontSites.clear();
	rankedOwnedSites.clear();
	maxExplore = -Float.MAX_VALUE;
    }
    
    @Override
    public void process(Site site) {
	cn.reset();
	MyBot.map.processCardinal(site.x,
				  site.y,
				  cn);

	if (site.owner == MyBot.ID)
	    rankedOwnedSites.add(site);
	else {
	    if (cn.nCount > 0)
		rankedFrontSites.add(site);
	    if (site.owner == 0)
		objectiveCandidates.add(site);
	    else if (cn.bCount > 0)
	        site.setBattle();
	}
	
	if (site.explore > maxExplore)
	    maxExplore = site.explore;
    }

    public void postProcess() {
	spreadBattle();
		
	pruneFront();

	spreadTacticalObjective();

	findObjectives();
    }

    private void spreadBattle() {
	for (int i = MyBot.ai.battles.nextSetBit(0); i != -1; i = MyBot.ai.battles.nextSetBit(i+1)) {
	    Site s = MyBot.map.getSite(i);
	    MyBot.map.processCardinal(s.x, s.y,
				      s1 -> {
					  if (s1.neutral() && (s1.units == 0)) {
					      Ring ring = MyBot.map.getRing(s1);
					      for (int r = 0; r < ring.layers.length; r++) {
						  float reduction = MyBot.tables.reductionSet[r+1];
						  TShortArrayList ringSites = ring.layers[r];
						  for (int q = 0; q < ringSites.size(); q++) {
						      short z = ringSites.getQuick(q);
						      Site ringSite = MyBot.map.getSite(z);
						      if (ringSite.units == 0) {
							  float v = s.defense * reduction;
							  if (v > ringSite.defense)
							      ringSite.defense = v;
							  v = s.damage * reduction;
							  if (v > ringSite.damage)
							      ringSite.damage = v;
						      }
						  }
					      }
					  }
				      });
	}

	for (Site s : rankedFrontSites) {
	    if (s.defense > 0)
		MyBot.map.processCardinal(s.x, s.y,
					  s1 -> {
					      if (s1.mine()) {
						  Ring ring = MyBot.map.getRing(s1);
						  for (int r = 0; r < ring.layers.length && r <= maxRadius * 0.8; r++) {
						      float reduction = MyBot.tables.reductionSet[r+1];
						      TShortArrayList ringSites = ring.layers[r];
						      for (int i = 0; i < ringSites.size(); i++) {
							  short z = ringSites.getQuick(i);
							  Site ringSite = MyBot.map.getSite(z);
							  float v = s.defense * reduction;
							  if (v > ringSite.defense)
							      ringSite.defense = v;
							  v = s.damage * reduction;
							  if (v > ringSite.damage)
							      ringSite.damage = v;
						      }
						  }					      
					      }
					  });    
	}
    }

    private void pruneFront() {

	for (Iterator<Cursor> cursor = pointers.iterator(); cursor.hasNext();)
	    if (cursor.next().refresh())
		cursor.remove();

	float maxExploreLocal = rankedFrontSites.first().explore;
	
	if (pointers.size() < 1)
	    pointers.add(new Cursor(rankedFrontSites.pollFirst()));
	
	for (Site s : rankedFrontSites) {
	    boolean allowed = true;
	    if (s.explore < maxExploreLocal * 0.85)
		break;
	    for (Cursor checker : pointers) {
		if (MyBot.map.manhattanDistance(checker.head, s) < 12) {
		    allowed = false;
		    break;
		}
	    }
	    if (allowed)
	        pointers.add(new Cursor(s));
	}
    }

    private void spreadTacticalObjective() {
	for (Cursor c : pointers) {
	    Site f = c.head;
	    MyBot.map.processCardinal(f.x, f.y,
				      s -> {
					  if (s.mine()) {
					      Ring ring = MyBot.map.getRing(s);
					      for (int cr = 0; cr <= maxRadius; cr++) {
						  TShortArrayList ringSites = ring.layers[cr];
						  for (int z = 0; z < ringSites.size(); z++) {
						      int i = ringSites.getQuick(z);
						      Site ringSite = MyBot.map.getSite(i);
						      float v = f.getValue() * MyBot.tables.reductionSet[cr];
						      if (v > ringSite.strength)
							  ringSite.strength = v;
						  }
					      }
					  }
				      });
	}
    }
    
    private void findObjectives() {	
	TreeSet<Site> tempRanked = new TreeSet<Site>();

	float threshold = 0.95f * maxExplore;
	
	for (Iterator<Site> cursor = objectiveCandidates.iterator(); cursor.hasNext();) {
	    Site o = cursor.next();
	    if (o.explore > threshold) {
		boolean add = true;
		for (Site s1 :  tempRanked) {
		    float distance = MyBot.map.manhattanDistance(s1, o);
		    if (distance < 5) {
			add = false;
			break;
		    }
		}
		if (add) {
		    o.setObjective();
		    tempRanked.add(o);
		}
	    }
	}

	for (Site o :  tempRanked) {
	    eop.setCenter(o);
	    MyBot.map.processCardinal(o.x, o.y, eop);
	}
    }
    
    private class CountNeigbhors implements ProcessSite {
	public int nCount = 0;
	public int bCount = 0;
	@Override
	public void process(Site site) {
	    if ((site.owner == 0) && (site.units == 0))
		bCount++;
	    else if (site.owner == MyBot.ID)
		nCount++;
	}

	public void reset() {
	    bCount = 0;
	    nCount = 0;
	}
    }

    private class ExpandObjectiveProcess implements ProcessSite {
	private float score;

	public void setCenter(Site center) {
	    this.score = 0.85f * center.explore;
	}
	
	@Override
	public void process(Site site) {
	    if (!site.objective() && (site.explore > score) && !site.mine()) {
	        site.setObjective();
		MyBot.map.processCardinal(site.x, site.y, eop);
	    }
	}
    }
}
