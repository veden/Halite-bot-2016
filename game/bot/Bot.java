package game.bot;

import java.util.HashSet;

import game.Site;
import game.bot.ai.Attack;
import game.bot.ai.Explore;
import game.bot.ai.Movement;
import game.bot.model.Player;

public class Bot extends Player {
    private static Attack attack;
    private static Explore explore;
    private static Movement movement;
    
    public HashSet<Site> unexplored = new HashSet<Site>();
    
    public Bot(byte owner) {
	super(owner);
    }

    @Override
    public void move() {
	
    }

    public void addUnexplored(Site s) {
	unexplored.add(s);
    }

    @Override
    public void reset() {
	super.reset();
	unexplored.clear();
    }
}
