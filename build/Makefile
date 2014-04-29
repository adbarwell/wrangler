.PHONY: all clean console compile compile_testfiles

all: 
	@echo "Nobody here but us chickens."

clean:
	rm testfiles/*.beam
	rm ../qc_test/*.beam

compile:
	@cd testfiles && erlc -W0 *.erl
	@cd ../qc_test && erlc -W0 *.erl

console: compile
	@exec erl -args_file ./priv/default.args
