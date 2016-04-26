REBAR = rebar3
DEPS = $(CURDIR)/deps

DIALYZER_OPTS = -Wunderspecs

# List dependencies that should be included in a cached dialyzer PLT file.
# DIALYZER_DEPS = deps/app1/ebin \
#                 deps/app2/ebin

DEPS_PLT = {{name}}.plt

ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       erts \
                       eunit \
                       gs \
                       hipe \
                       inets \
                       kernel \
                       mnesia \
                       observer \
                       public_key \
                       runtime_tools \
                       ssl \
                       stdlib \
                       syntax_tools \
                       tools \
                       webtool \
                       xmerl

all: compile eunit dialyzer

# Clean ebin and .eunit of this project
clean:
	$(REBAR) clean skip_deps=true

# Clean this project and all deps
allclean:
	$(REBAR) clean

compile: $(DEPS)
	$(REBAR) compile

$(DEPS):
	$(REBAR) get-deps

# Full clean and removal of all deps. Remove deps first to avoid
# wasted effort of cleaning deps before nuking them.
distclean:
	@rm -rf deps $(DEPS_PLT)
	$(REBAR) clean

eunit:
	$(REBAR) eunit

test: eunit

# Only include local PLT if we have deps that we are going to analyze
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer: ~/.dialyzer_plt
	@dialyzer $(DIALYZER_OPTS) -r ebin | fgrep -v -f dialyzer.ignore
else
dialyzer: ~/.dialyzer_plt $(DEPS_PLT)
	@dialyzer $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin | fgrep -v -f dialyzer.ignore

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

doc:
	$(REBAR) doc skip_deps=true

.PHONY: all compile eunit test dialyzer clean allclean distclean doc