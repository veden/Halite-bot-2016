package logic.process;

import java.util.ArrayList;

import logic.world.Tile;

public interface ProcessTile {
    public void process(Tile t, ArrayList<Tile> neighbors);
}
