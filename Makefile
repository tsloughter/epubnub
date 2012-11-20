# -*- mode: Makefile; fill-column: 80; comment-column: 75; -*-

ERL = $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR=./rebar

EPUBNUB_PLT=$(CURDIR)/.depsolver_plt

.PHONY: dialyzer typer clean distclean

compile:
	@./rebar get-deps compile

$(EPUBNUB_PLT):
	dialyzer --output_plt $(EPUBNUB_PLT) --build_plt \
		--apps erts kernel stdlib crypto public_key -r deps --fullpath

dialyzer: $(EPUBNUB_PLT)
	dialyzer --plt $(EPUBNUB_PLT) -pa deps/* --src src

typer: $(EPUBNUB_PLT)
	typer --plt $(EPUBNUB_PLT) -r ./src

clean:
	$(REBAR) clean

distclean: clean
	rm $(EPUBNUB_PLT)
	rm -rvf $(CURDIR)/deps/*
