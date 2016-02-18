REBAR=rebar3 $(REBAR_ARGS)

.PHONY: all
all: compile

.PHONY: compile
compile: slex
	@$(REBAR) compile

.PHONY: slex
slex: src/erlydtl_scanner.erl

src/erlydtl_scanner.erl: src/erlydtl_scanner.slex
	@$(REBAR) slex compile

.PHONY: check
check: test dialyze

.PHONY: test
test:
	@$(REBAR) eunit

.PHONY: dialyze
dialyze: compile
	@$(REBAR) dialyzer

.PHONY: clean
clean:
	@$(REBAR) clean

.PHONY: realclean
realclean: clean
	rm -rf _build

.PHONY: committed
committed:
	@git diff --no-ext-diff --quiet --exit-code || { echo "there are uncommitted changes in the repo." ; false ;}

.PHONY: committed
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
