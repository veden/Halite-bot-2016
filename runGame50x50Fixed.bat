del *.hlt
del *.log
del *.class

javac MyBot.java
javac RandomBot.java
cd v2
javac MyBot.java
cd ..
z:/halite.exe -q -d "50 50" -s 572024590 "java MyBot" "cd v2 && java MyBot" "java RandomBot"
pause
