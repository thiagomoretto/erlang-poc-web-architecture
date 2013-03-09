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
		-pa $(PWD)/core/shared/ebin \
		-pa $(PWD)/core/base_frontend/ebin \
		-pa $(PWD)/app/frontends/posts_frontend/ebin \
		-pa $(PWD)/deps/*/ebin \
		-boot start_sasl \
		-config $(PWD)/core/base_frontend/priv/app.config \
		-s reloader -s frontend

services-start: app
	exec erl \
		-pa $(PWD)/core/shared/ebin \
		-pa $(PWD)/core/base_service/ebin \
		-pa $(PWD)/app/services/posts_service/ebin \
		-pa $(PWD)/deps/*/ebin \
		-boot start_sasl \
		-config $(PWD)/core/base_service/priv/app.config \
		-s reloader -s core

proxystart:
	@haproxy -f dev.haproxy.conf

api-tests:
	$(shell $(PYTHON) -m unittest discover test '*_tests.py' -v)