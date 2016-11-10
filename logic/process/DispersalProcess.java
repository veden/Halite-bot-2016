package logic.process;

import java.util.ArrayList;

import logic.constant.Constants;
import logic.structure.Map;
import logic.structure.Tile;

public class DispersalProcess implements ProcessTile {

    public void process(Tile tile, ArrayList<Tile> neighbors) {
	tile.production *= Constants.SITE_PERSISTANCE;
	tile.strength *= Constants.SITE_PERSISTANCE;
	tile.defense *= Constants.SITE_PERSISTANCE;

	if (tile.production < 0.01)
	    tile.production = 0;
	if (tile.strength < 0.01)
	    tile.strength = 0;
	if (tile.defense < 0.01)
	    tile.defense = 0;
	
	float amountDispersedProduction = tile.production * Constants.SITE_DISPERSAL;
	float amountDispersedStrength = tile.strength * Constants.SITE_DISPERSAL;
	float amountDispersedDefense = tile.defense * Constants.SITE_DISPERSAL;

	for (int i = 0; i < neighbors.size(); i++) {
	    Tile neighbor = neighbors.get(i);
	    neighbor.production += amountDispersedProduction;
	    neighbor.strength += amountDispersedStrength;
	    neighbor.defense += amountDispersedDefense;
	}

	tile.production -= amountDispersedProduction * 4;
	tile.strength -= amountDispersedStrength * 4;
	tile.defense -= amountDispersedDefense * 4;
    }
}
