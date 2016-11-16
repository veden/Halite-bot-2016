del *.hlt
del *.log
del *.class

javac MyBot.java
javac RandomBot.java
z:/halite.exe -q -d "50 50" -s 572024590 "java MyBot" "java RandomBot" "java RandomBot"
pause
