
generate_traces:
	./rebar3 compile
	erl -pa `./rebar3 path` -noshell -eval 'erltv:trace_repl_scenarios(["priv/learn-you-some-erlang"])' -eval 'init:stop()'