HOST=$(shell erl -noinput -sname console -eval "{ok, H} = inet:gethostname(), io:format(\"~s\",[H]), erlang:halt()")

all: test

extras/rebar_epitest/ebin/rebar_epitest.beam:
	@cd extras/rebar_epitest && ../../rebar compile

compile: extras/rebar_epitest/ebin/rebar_epitest.beam
	@./rebar compile

test: compile extras/rebar_epitest/ebin/rebar_epitest.beam 
	@./rebar epitest

clean:
	@cd extras/rebar_epitest && ../../rebar clean
	@./rebar clean

atest:
	@EPITEST_ATTACH=1 ./rebar epitest

attach:
	@erl -sname console -remsh test@$(HOST)
