.PHONY: deps app clean doc
PROJECT = erl-4inline

DIALYZER = dialyzer
REBAR = ./rebar

all: app

# Application.

deps:
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile

rel: app
	@$(REBAR) generate

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

doc:
	@$(REBAR) doc

tests: app
	@$(REBAR) compile eunit skip_deps=true
