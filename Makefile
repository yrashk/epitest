all: test

compile:
	@./rebar compile

test: compile
	@./rebar epitest

clean:
	@./rebar clean
