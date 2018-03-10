-module(vt_web).
-export([
  create_user_and_return_access_key/0
]).



create_user_and_return_access_key() ->
  WebappNode = vt:webapp_node(),
  {ok, UserId, AccessKey} = rpc:call(WebappNode, 'Elixir.Vision.Test', create_user_and_return_access_key, []),
  {ok, UserId, AccessKey}.