ERL=erl
ERLC=erlc
REBAR=./rebar $(REBAR_ARGS)

all: compile

compile: check-slex get-deps
	@$(REBAR) compile

check-slex: src/erlydtl_scanner.erl
src/erlydtl_scanner.erl: src/erlydtl_scanner.slex
	@echo Notice: $@ is outdated by $<, consider running "'make slex'".

get-deps:
	@$(REBAR) get-deps

update-deps:
	@$(REBAR) update-deps

.PHONY: tests
tests: src/erlydtl_parser.erl
	@$(REBAR) eunit

check: tests dialyze

## dialyzer
PLT_FILE = ~/erlydtl.plt
PLT_APPS ?= kernel stdlib compiler erts eunit syntax_tools
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns \
		-Wunderspecs --verbose --fullpath
.PHONY: dialyze
dialyze:
	@dialyzer --plt $(PLT_FILE) $(DIALYZER_OPTS) ebin || [ $$? -eq 2 ];

## In case you are missing a plt file for dialyzer,
## you can run/adapt this command
.PHONY: plt
plt:
# we need to remove second copy of file
	rm -f deps/merl/priv/merl_transform.beam
	@echo "Building PLT, may take a few minutes"
	@dialyzer --build_plt --output_plt $(PLT_FILE) --apps \
		$(PLT_APPS) deps/* || [ $$? -eq 2 ];

clean:
	@echo "Clean merl..." ; $(MAKE) -C deps/merl clean
	@$(REBAR) -C rebar-slex.config clean
	rm -fv erl_crash.dump

# rebuild any .slex files as well..  not included by default to avoid
# the slex dependency, which is only needed in case the .slex file has
# been modified locally.
slex: REBAR_DEPS ?= get-deps update-deps
slex: slex-compile

slex-skip-deps: REBAR_DEPS:=
slex-skip-deps: slex-compile

slex-compile:
	@$(REBAR) -C rebar-slex.config $(REBAR_DEPS) compile

shell:
	@$(ERL) -pz ebin deps/*/ebin


# this file must exist for rebar eunit to work
# but is only built when running rebar compile
src/erlydtl_parser.erl: compile
