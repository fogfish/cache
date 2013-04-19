.PHONY: deps test

BB=../basho_bench

all: rebar deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean
	rm -rf test.*-temp-data

distclean: clean 
	./rebar delete-deps

test: all
	./rebar skip_deps=true eunit

docs:
	./rebar skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c ./ebin

run:
	erl -name cache@127.0.0.1 -setcookie nocookie -pa ./deps/*/ebin -pa ./ebin

rebar:
	curl -O http://cloud.github.com/downloads/basho/rebar/rebar
	chmod ugo+x rebar

benchmark:
	$(BB)/basho_bench priv/cache.benchmark
	$(BB)/priv/summary.r -i tests/current
	open tests/current/summary.png


