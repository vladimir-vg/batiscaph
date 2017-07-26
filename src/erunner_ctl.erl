-module(erunner_ctl).
-export([start/2]).



start(Dir, Id) ->
  Erl = case os:find_executable("erl") of
    false -> "/usr/bin/erl";
    ErlPath when is_list(ErlPath) -> ErlPath
  end,

  {ok, MasterPort} = application:get_env(espace, http_port),
  Paths = string:tokens(os:cmd("cd ../erunner && ./rebar3 path"), " "),
  Args0 = lists:concat([["-pa", P] || P <- Paths]),
  Args1 = Args0 ++ ["-noshell", "-s", "erunner"], %  "-sname", Id,
  Env = [{"MASTER_PORT", integer_to_list(MasterPort)}, {"SHELL_SESSION_ID", binary_to_list(Id)}],
  DirPath = filename:join([code:priv_dir(espace), "scenarios", Dir, Id]),
  ok = filelib:ensure_dir(DirPath),
  ok = file:make_dir(DirPath),
  Opts = [{args, Args1},{env,Env},{cd,DirPath}],
  Port = erlang:open_port({spawn_executable, Erl}, Opts),
  {ok, Port}.