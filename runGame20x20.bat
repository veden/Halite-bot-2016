del *.hlt
del *.log
del *.class

javac MyBot.java
javac RandomBot.java
z:/halite.exe -q -d "20 20" "java MyBot" "java RandomBot" "java RandomBot"
pause
