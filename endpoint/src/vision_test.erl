-module(vision_test).
-export([
  subscribe_to_session/2,
  get_subscribers_for_user/1,

  subscribe_to_first_guest/1,
  notify_guest_info_subscribers/1
]).



subscribe_to_first_guest(Pid) ->
  ets:insert(test_subscriptions, {first_guest_info, Pid}),
  ok.

notify_guest_info_subscribers(Info) ->
  case ets:take(test_subscriptions, first_guest_info) of
    [] -> ok;
    Subs ->
      [Pid ! {from_probe, Info} || {first_guest_info, Pid} <- Subs],
      ok
  end.



subscribe_to_session(Pid, #{user_id := UserId}) when is_integer(UserId) ->
  ets:insert(test_subscriptions, {{user_id, UserId}, Pid}),
  ok.

get_subscribers_for_user(UserId) ->
  case ets:take(test_subscriptions, {user_id, UserId}) of
    [] -> none;
    Subs ->
      Pids = [Pid || {{user_id, _}, Pid} <- Subs],
      {ok, Pids}
  end.
