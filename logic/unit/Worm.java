package logic.unit;

import game.Move;

public class Worm {

    private int strength;
    private int x;
    private int y;
    
    public Worm(int x, int y) {
	this.x = x;
	this.y = y;
    }

    public int predictStrength(int x, int y) {
	return -1;
    }

    public Move pursue(int x, int y) {
	return null;
    }
}
