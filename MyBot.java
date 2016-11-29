import game.Harness;

public class MyBot {
    
    public static void main(String[] args) throws java.io.IOException {
        Harness.startBot();
	
	while(true) {
	    Harness.readMap();
	    Harness.sendMoves();
	}
    }
}
