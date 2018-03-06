#!/bin/bash
erl -pa `./rebar3 path` -s vision_cmds reset_clickhouse -noshell -eval 'init:stop()'
