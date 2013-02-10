ERL?=erl

.PHONY: deps

all: deps
	@./rebar compile

app:
	@./rebar compile skip_deps=true

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

webstart: app
	exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl \
		-config $(PWD)/apps/frontend/priv/app.config \
		-s reloader -s frontend

corestart: app
	exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl \
		-config $(PWD)/apps/core/priv/app.config \
		-s reloader -s core

proxystart:
	@haproxy -f dev.haproxy.conf
