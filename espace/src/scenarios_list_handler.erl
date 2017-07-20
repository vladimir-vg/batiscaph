-module(scenarios_list_handler).
-include_lib("kernel/include/file.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  {ok, Req, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.



handle(Req, State) ->
  case get_items_list(1000) of
    {error, Reason} ->
      Body = iolist_to_binary(io_lib:format("~p", [Reason])),
      {ok, Req1} = cowboy_req:reply(500, [{<<"content-type">>, <<"text/plain">>}], Body, Req),
      {ok, Req1, State};

    {ok, Items} ->
      Body = jsx:encode(Items),
      {ok, Req1} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Body, Req),
      {ok, Req1, State}
  end.



% get sorted list of scenarios
% scenarios which dir doesn't look like a date go first (like 'learn-you-some-erlang')
get_items_list(MaxItems) ->
  DBName = espace:get_prop(clickhouse_dbname),
  SQL = iolist_to_binary([
    "SELECT instance_id, MIN(toUInt64(at)) AS min_at\n",
    "FROM `", DBName, "`.events\n",
    "GROUP BY instance_id\n",
    "ORDER BY min_at DESC\n",
    "LIMIT ", integer_to_binary(MaxItems), "\n",
    "FORMAT TabSeparatedWithNamesAndTypes\n"
  ]),
  {ok, Body} = clickhouse:execute(SQL),
  {ok, Items} = clickhouse:parse_rows(maps, Body),
  {ok, Items}.
  % Path = code:priv_dir(espace) ++ "/scenarios",
  % case file:list_dir(Path) of
  %   {error, Reason} -> {error, Reason};
  %   {ok, Filenames} ->
  %     {ok, DateDirs, NonDateDirs} = filter_filenames(Path, Filenames),
  %     Dirs = lists:sort(NonDateDirs) ++ lists:sort(fun (A,B) -> A > B end, DateDirs),
  %     {ok, Items} = collect_scenarios_from(MaxItems, Path, Dirs),
  %     {ok, Items}
  % end.



% filter_filenames(Path, Filenames) ->
%   filter_filenames(Path, Filenames, [], []).
% 
% filter_filenames(_Path, [], DateDirs, NonDateDirs) ->
%   {ok, DateDirs, NonDateDirs};
% 
% filter_filenames(Path, [Filename | Rest], DateDirs, NonDateDirs) ->
%   case file:read_file_info(Path ++ "/" ++ Filename) of
%     {ok, #file_info{type = directory}} ->
%       case re:run(Filename, "^\\d\\d\\d\\d-\\d\\d-\\d\\d$") of
%         {match, _} -> filter_filenames(Path, Rest, [Filename | DateDirs], NonDateDirs);
%         nomatch -> filter_filenames(Path, Rest, DateDirs, [Filename | NonDateDirs])
%       end;
%     {ok, _} -> filter_filenames(Path, Rest, DateDirs, NonDateDirs);
%     {error, _Reason} -> filter_filenames(Path, Rest, DateDirs, NonDateDirs)
%   end.
% 
% 
% 
% collect_scenarios_from(ItemsLeft, Path, Dirs) ->
%   collect_scenarios_from(ItemsLeft, Path, Dirs, []).
% 
% collect_scenarios_from(0, _Path, _Dirs, Acc) -> {ok, lists:reverse(Acc)};
% collect_scenarios_from(_ItemsLeft, _Path, [], Acc) -> {ok, lists:reverse(Acc)};
% 
% collect_scenarios_from(ItemsLeft, Path, [Dir | Rest], Acc) ->
%   DirPath = Path ++ "/" ++ Dir,
% 
%   case file:list_dir(DirPath) of
%     {error, _Reason} -> collect_scenarios_from(ItemsLeft, Path, Rest, Acc);
%     {ok, Items} ->
%       case collect_scenarios0(ItemsLeft, DirPath, Items) of
%         {ok, ItemsLeft1, []} -> collect_scenarios_from(ItemsLeft1, Path, Rest, Acc);
%         {ok, ItemsLeft1, JsonItems} ->
%           JsonItem = #{dir => list_to_binary(Dir), items => JsonItems},
%           collect_scenarios_from(ItemsLeft1, Path, Rest, [JsonItem | Acc])
%       end
%   end.
% 
% 
% 
% collect_scenarios0(ItemsLeft, DirPath, Items) ->
%   collect_scenarios0(ItemsLeft, DirPath, Items, []).
% 
% collect_scenarios0(0, _DirPath, _Items, Acc) -> {ok, 0, lists:sort(Acc)};
% collect_scenarios0(ItemsLeft, _DirPath, [], Acc) -> {ok, ItemsLeft, lists:sort(Acc)};
% collect_scenarios0(ItemsLeft, DirPath, [Dirname | Items], Acc) ->
%   DirPath2 = DirPath ++ "/" ++ Dirname,
%   CsvPath = DirPath ++ "/" ++ Dirname ++ "/" ++ Dirname ++ ".csv",
%   % such dir exist and has csv file with same basename
%   case {file:read_file_info(DirPath2), file:read_file_info(CsvPath)} of
%     {{ok, #file_info{type = directory}}, {ok, #file_info{type = regular}}} ->
%       collect_scenarios0(ItemsLeft-1, DirPath, Items, [list_to_binary(Dirname) | Acc]);
%     _ ->
%       collect_scenarios0(ItemsLeft, DirPath, Items, Acc)
%   end.

