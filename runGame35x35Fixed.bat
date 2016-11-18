del *.hlt
del *.log
del *.class

javac MyBot.java
javac RandomBot.java
cd v2
javac MyBot.java
cd ..
z:/halite.exe -q -d "35 35" -s 3895162942 "java MyBot" "cd v2 && java MyBot" "java RandomBot"
pause
