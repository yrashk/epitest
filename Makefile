all: test

compile:
	@./rebar compile

test: 
	@./rebar epitest

clean:
	@./rebar clean
