-module(shell_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  welcome_output/1
]).



% Tests in this suite check correct behaviour of config
% transmission and application



all() ->
  [welcome_output].



init_per_suite(Config) ->
  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  ok = vt:ensure_started(#{logdir => PrivDir}),

  Config.

end_per_suite(Config) ->
  Config.



welcome_output(Config) ->
  {ok, UserId, AccessKey} = vt_web:create_user_and_return_access_key(),

  % subscribe for communication
  ok = vt_endpoint:subscribe_to_session(#{user_id => UserId}),

  PrivDir = list_to_binary(proplists:get_value(priv_dir, Config)),
  {ok, _AppContainer} = vt:start_docker_container(?MODULE, <<"vision-test/erlang_app1:latest">>, #{
    host_network => true, logdir => PrivDir,
    <<"VISION_PROBE_ENDPOINT_URL">> => vt:endpoint_url(),
    <<"VISION_PROBE_ACCESS_KEY">> => AccessKey
  }),

  InstanceId =
    receive {probe_connected, Id} -> Id
    after 5000 -> error(probe_connection_timeout)
    end,

  {response, _, apply_config, ok} = vt_endpoint:received_from_probe(apply_config),

  {ok, Ws} = vt_endpoint:ws_connect(),

  ok = vt_endpoint:ws_send(Ws, connect_to_shell, InstanceId),
  connected_to_shell = vt_endpoint:ws_delivered(Ws, connected_to_shell),

  ok = vt_endpoint:ws_send(Ws, subscribe_to_instance, InstanceId),
  % _ = vt_endpoint:ws_delivered(Ws, delta),
  #{<<"shell-commands">> := Cmds} = vt_endpoint:ws_delivered(Ws, delta),

  [{_, Cmd}] = maps:to_list(Cmds),
  #{<<"FirstOutputAt">> := _, <<"LastOutputAt">> := _, <<"Output">> := Outputs} = Cmd,
  [{_, Output1}] = maps:to_list(Outputs),
  #{<<"At">> := _, <<"Text">> := <<"Eshell V9.2\n">>} = Output1,

  ok.
