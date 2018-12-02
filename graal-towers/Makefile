all: compile

.PHONY:
compile:
	sbt run -java-home /home/gardei/Hacking/graal/graalvm-ce-1.0.0-rc9

.PHONY:
debug:
	sbt run -java-home /home/gardei/Hacking/graal/graalvm-ce-1.0.0-rc9 -J-Dgraal.PrintCompilation=true
