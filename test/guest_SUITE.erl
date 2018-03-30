-module(guest_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  receive_version/1
]).



% Tests in this suite checks behaviour of the client
% in case if it was not supplied by any credentials.



all() ->
  [receive_version].



init_per_suite(Config) ->
  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_started(#{logdir => PrivDir}),
  Config.

end_per_suite(Config) ->
  Config.



receive_version(Config) ->
  ok = vt_endpoint:subscribe_to_first_guest(),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  {ok, _} = vt:start_docker_container(?MODULE, <<"vision-test/erlang_app1:latest">>, #{
    host_network => true, logdir => PrivDir,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url()
  }),

  {summary_info, #{
    probe_version := <<"0.1.0">>,
    dependency_in := [{<<"erlang_app1">>, <<"1.2.3-test1">>}]
  }} = vt_endpoint:received_from_probe(summary_info),

  ok.
