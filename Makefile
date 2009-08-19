VSN=$(shell git log --pretty=format:%h -n 1)
MAKEARG=[{d,vsn,\"${VSN}\"}]
PWD=$(shell pwd)

all: ebin/epitest.app

ebin/epitest.app: ebin/epitest.app.src compile
	@cat ebin/epitest.app.src | sed s/%vsn%/$(VSN)/g > ebin/epitest.app

compile:
	@erl -noshell -eval "make:all($(MAKEARG))" -s erlang halt

clean:
	rm -rf ebin/*.app ebin/*.beam

vsn:
	@echo $(VSN)
