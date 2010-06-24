HOST=$(shell erl -noinput -sname console -eval "{ok, H} = inet:gethostname(), io:format(\"~s\",[H]), erlang:halt()")

all: test

compile:
	@./rebar compile

test: 
	@./rebar epitest

clean:
	@./rebar clean

atest:
	@EPITEST_ATTACH=1 ./rebar epitest

attach:
	@erl -sname console -remsh test@$(HOST)
