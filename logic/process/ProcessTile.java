package logic.process;

import java.util.ArrayList;

import logic.structure.Tile;

public interface ProcessTile {
    public void process(Tile t, ArrayList<Tile> neighbors);
}
