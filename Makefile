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
tests: export EXTRA_CONFIG=rebar-tests.config
tests: src/erlydtl_parser.erl
	@$(REBAR) eunit

check: tests dialyze

## dialyzer
PLT_FILE = ~/erlydtl.plt
PLT_APPS ?= kernel stdlib compiler erts eunit syntax_tools
DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns \
		-Wunderspecs --verbose --fullpath
.PHONY: dialyze
dialyze: compile
	@[ -f $(PLT_FILE) ] || $(MAKE) plt
	@dialyzer --plt $(PLT_FILE) $(DIALYZER_OPTS) ebin || [ $$? -eq 2 ];

## In case you are missing a plt file for dialyzer,
## you can run/adapt this command
.PHONY: plt
plt: compile
# we need to remove second copy of file
	rm -f deps/merl/priv/merl_transform.beam
	@echo "Building PLT, may take a few minutes"
	@dialyzer --build_plt --output_plt $(PLT_FILE) --apps \
		$(PLT_APPS) deps/* || [ $$? -eq 2 ];

clean:
	@[ ! -d deps/merl ] || { echo "Clean merl..." ; $(MAKE) -C deps/merl clean ;}
	@$(REBAR) -C rebar-slex.config clean
	rm -fv erl_crash.dump

really-clean: clean
	rm -f $(PLT_FILE)

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

committed:
	@git diff --no-ext-diff --quiet --exit-code || { echo "there are uncommitted changes in the repo." ; false ;}

release: committed check
	@{														      \
		V0=$$(grep vsn src/erlydtl.app.src | sed -e 's/.*vsn,.*"\(.*\)".*/\1/')					   && \
		V1=$$(grep '##' -m 1 NEWS.md | sed -e 's/##[^0-9]*\([0-9.-]*\).*/\1/')					   && \
		read -e -p "OK, all tests passed, current version is $$V0, which version should we release now? ($$V1)" V2 && \
		: $${V2:=$$V1}												   && \
		echo "$$V2 it is..."											   && \
		sed -i -e 's/vsn,.*}/vsn, "'$$V2'"}/' src/erlydtl.app.src						   && \
		git ci -m "release v$$V2" src/erlydtl.app.src								   && \
		git tag $$V2												   && \
		echo 'Updated src/erlydtl.app.src and tagged, run `git push origin master --tags` when ready'                 \
	;}
