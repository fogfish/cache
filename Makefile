.PHONY: deps test

ifeq ($(id),)
export id=cache
endif

FLAGS=\
        -name ${id}@127.0.0.1 \
        -setcookie nocookie \
        -pa ./deps/*/ebin \
        -pa ./examples/*/ebin \
        -pa ./ebin \
        +K true +A 160 -sbt ts

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
	erl ${FLAGS}

rebar:
	curl -O http://cloud.github.com/downloads/basho/rebar/rebar
	chmod ugo+x rebar

benchmark:
	$(BB)/basho_bench -N bb@127.0.0.0 -C nocookie -J ${id}@127.0.0.1 priv/${id}.benchmark
	$(BB)/priv/summary.r -i tests/current
	open tests/current/summary.png

# benchmark:
# 	$(BB)/basho_bench -N bb@127.0.0.0 -C nocookie -J ${id}@127.0.0.1 priv/${id}.benchmark
# 	$(BB)/priv/summary.r -i tests/current
# 	open tests/current/summary.png

