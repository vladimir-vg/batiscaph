-module(batiscaph_probe_util).
-export([
  bin_to_hex/1,
  apps_have_dependency/1,
  format_mfa/1, format_reason/1, format_term/1
]).



% stolen from here:
% https://stackoverflow.com/a/29819282/614661
bin_to_hex(Bin) when is_binary(Bin) ->
  << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.



apps_have_dependency(DepAtom) ->
  Apps = application:which_applications(),

  lists:foldl(fun ({AppId, _, _} = App, Acc) ->
    case application:get_key(AppId, applications) of
      undefined -> Acc;
      {ok, Deps} ->
        case lists:member(DepAtom, Deps) of
          true -> [App | Acc];
          false -> Acc
        end
    end
  end, [], Apps).



format_mfa({M, F, A}) ->
  iolist_to_binary([
    atom_to_binary(M,latin1), ":",
    atom_to_binary(F,latin1), "/",
    integer_to_binary(erlang:length(A))
  ]).

% TODO: io_lib:format is known to be slow,
% also it might be wrong to reveal whole term to service
format_reason(Reason) ->
  iolist_to_binary(io_lib:format("~p", [Reason])).

format_term(Term) ->
  iolist_to_binary(io_lib:format("~p", [Term])).

