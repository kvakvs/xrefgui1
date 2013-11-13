REBAR=./rebar

.PHONY: all
all: rebar deps

rebar:
	wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar

deps:
	$(REBAR) get-deps