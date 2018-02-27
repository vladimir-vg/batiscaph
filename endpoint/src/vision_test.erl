-module(vision_test).
-export([
  subscribe_to_first_session/2,
  get_subscriber_for_user/1,

  subscribe_to_first_guest/1,
  notify_guest_info_subscriber/1
]).



subscribe_to_first_guest(Pid) ->
  ets:insert(test_subscriptions, {first_guest_info, Pid}),
  ok.

notify_guest_info_subscriber(Info) ->
  case ets:take(test_subscriptions, first_guest_info) of
    [] -> ok;
    [{first_guest_info, Pid}] -> Pid ! {from_probe, Info}, ok
  end.



subscribe_to_first_session(Pid, #{user_id := UserId}) ->
  ets:insert(test_subscriptions, {{user_id, UserId}, Pid}),
  ok.

get_subscriber_for_user(UserId) ->
  case ets:take(test_subscriptions, {user_id, UserId}) of
    [{{user_id, UserId}, Pid}] -> {ok, Pid};
    [] -> none
  end.
