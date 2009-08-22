VSN=$(shell git log --pretty=format:%h -n 1)
MAKEARG=[{d,vsn,\"${VSN}\"}]
PWD=$(shell pwd)

all: ebin/epitest.app

ebin/epitest.app: ebin/epitest.app.src compile
	@cat ebin/epitest.app.src | sed s/%vsn%/$(VSN)/g > ebin/epitest.app

compile:
	@erl -pa ebin -noshell -eval "make:all($(MAKEARG))" -s erlang halt

selftest: compile
	@mkdir -p _tests
	@erl -noshell -epitest dir \"_tests\" -sname epitest -pa t ebin -s epitest -s epitest modules selftest -s epitest_console_logger -s epitest run 

clean:
	rm -rf ebin/*.app ebin/*.beam

vsn:
	@echo $(VSN)
