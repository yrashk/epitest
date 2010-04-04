all: test

compile:
	@./rebar compile

test: compile
	@./rebar epitest
