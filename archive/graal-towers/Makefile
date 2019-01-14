all: compile

ifeq "$(T_GRAAL_HOME)" ""
T_GRAAL_HOME = /home/gardei/Hacking/graal/graalvm-ce-1.0.0-rc9
endif

.PHONY:
compile:
	sbt compile -java-home $(T_GRAAL_HOME)

.PHONY:
debug:
	sbt run -java-home $(T_GRAAL_HOME) -J-Dgraal.PrintCompilation=true