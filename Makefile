.PHONY: deps compile clean doc
PROJECT = erl-4inline

DIALYZER = dialyzer
REBAR = ./rebar

all: compile

# Application.

deps:
	@$(REBAR) get-deps

compile: deps
	@$(REBAR) compile

rel: compile
	@$(REBAR) generate

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

doc:
	@$(REBAR) doc

test: compile
	@$(REBAR) compile eunit skip_deps=true
