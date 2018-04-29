#!/bin/bash
erl -pa `./rebar3 path` -s batiscaph_cmds reset_storage -noshell -eval 'init:stop()'
