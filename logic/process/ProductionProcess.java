package logic.process;

import java.util.ArrayList;

import logic.structure.Map;
import logic.structure.Tile;

public class ProductionProcess implements ProcessTile {

    public void process(Tile tile, ArrayList<Tile> neighbors) {
	float generator = tile.getProductionGenerator();
	tile.production = (generator * Tile.getProductionScaling());
    }
}
