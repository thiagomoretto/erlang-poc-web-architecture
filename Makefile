ERL?=erl
PYTHON=/usr/local/Cellar/python/2.7.3/bin/python

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
	exec erl \
		-pa $(PWD)/apps/app/ebin \
		-pa $(PWD)/apps/shared/ebin \
		-pa $(PWD)/apps/frontend/ebin \
		-pa $(PWD)/deps/*/ebin \
		-boot start_sasl \
		-config $(PWD)/apps/frontend/priv/app.config \
		-s reloader -s frontend

corestart: app
	exec erl \
		-pa $(PWD)/apps/app/ebin \
		-pa $(PWD)/apps/shared/ebin \
		-pa $(PWD)/apps/core/ebin \
		-pa $(PWD)/deps/*/ebin \
		-boot start_sasl \
		-config $(PWD)/apps/core/priv/app.config \
		-s reloader -s core

proxystart:
	@haproxy -f dev.haproxy.conf

api-tests:
	$(shell $(PYTHON) -m unittest discover test '*_tests.py' -v)