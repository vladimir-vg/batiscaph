-module(vision_test).
-export([subscribe_to_first_guest/1, notify_guest_info_subscriber/1]).



subscribe_to_first_guest(Pid) ->
  ets:insert(test_subscriptions, {first_guest_info, Pid}),
  ok.

notify_guest_info_subscriber(Info) ->
  case ets:lookup(test_subscriptions, first_guest_info) of
    [] -> ok;
    [{first_guest_info, Pid}] -> Pid ! {from_probe, Info}, ok
  end.