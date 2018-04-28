#!/bin/bash
erl -pa `./rebar3 path` -s batiscaph_cmds reset_clickhouse -noshell -eval 'init:stop()'
