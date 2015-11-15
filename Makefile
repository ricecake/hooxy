REBAR := `pwd`/rebar3

all: test release

compile:
	@$(REBAR) compile

test:
	@$(REBAR) do xref, dialyzer, eunit, cover

release:
	@$(REBAR) release

clean:
	@$(REBAR) clean

tarball:
	@$(REBAR) as prod tar

.PHONY: release test all compile clean
