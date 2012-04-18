.PHONY: deps docs

all: check-deps
	@./rebar compile

deps:
	@./rebar get-deps

check-deps:
	@./rebar check-deps

app.config:
	@cp app.config.orig app.config

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@./rebar skip_deps=true doc

test: test-eunit

PLT_NAME=.appenv_dialyzer.plt

$(PLT_NAME):
	@ERL_LIBS=deps dialyzer --build_plt --output_plt .appenv_dialyzer.plt \
		--apps kernel stdlib || true

dialyze: $(PLT_NAME)
	@dialyzer ebin --plt $(PLT_NAME) --no_native \
		-Werror_handling -Wrace_conditions -Wunderspecs

