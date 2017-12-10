-module(batiscaph_suite_transform).
-export([parse_transform/2]).



%%% This parse_transform takes testcases with -batiscaph_steps(). attribute
%%% and replaces their body with batiscaph_shell call, with AST.
%%% Basically just quotes body of the testcase for future step by step execution
%%% by batiscaph_shell.



% resulting module maybe be constituted from different files
% using inlude_lib directives
% need to keep track of current context files, and lines in those files
% to provide correct lines for evaluated expressions
-record(source_file, {
  path :: binary(),
  prev_lines = [] :: [binary()], % already extracted lines
  left_content :: binary() % unparsed content
}).

-record(suite_trans, {
  suite :: atom(),
  testcases :: [atom()],
  local_finder_name :: atom(),
  current_file :: binary(),
  files = #{} :: #{Path :: binary() => #source_file{}}
}).



parse_transform(Forms, _Options) ->
  case get_steps_attr_value(Forms) of
    undefined -> Forms;
    {ok, Attr} ->
      Testcases = calculate_testcases_to_wrap(Attr, Forms),
      {eof, EofLine} = lists:last(Forms),
      % generate secret function that provides access to local functions in suite
      % later append it at the end of file
      % this required for proper step-by-step execution by erl_eval
      {ok, LocalFinderName, LocalFunHandler} = generate_local_fun_finder(EofLine, Forms),
      % io:format("forms:~n~p~n", [Forms]),
      [Suite] = [M || {attribute,_,module,M} <- Forms],
      State = #suite_trans{suite = Suite, testcases = Testcases, local_finder_name = LocalFinderName},

      % Use Erlang/OTP private module to expand record expressions
      % into plain tuple expressions, to make it possible for erl_eval
      % to execute.
      %
      % This can break in future, if Erlang team decides to change erl_expand_records
      Forms0 = erl_expand_records:module(Forms, []),

      case wrap_functions(Forms0, State) of
        Forms0 -> Forms0; % no changes made, no need for local fun handler
        Forms1 when Forms1 =/= Forms0 ->
          % insert definiton of local fun handler at the end of the file
          [{eof, EofLine} | Rest] = lists:reverse(Forms1),
          lists:reverse([{eof, EofLine}, LocalFunHandler | Rest])
      end
  end.



get_steps_attr_value([]) -> undefined;
get_steps_attr_value([{attribute,_Line,batiscaph_steps,Value} | _Forms]) -> {ok, Value};
get_steps_attr_value([_ | Forms]) -> get_steps_attr_value(Forms).



calculate_testcases_to_wrap(all, Forms) ->
  calculate_testcases_from_all(Forms);
calculate_testcases_to_wrap(Testcases, _Forms) when is_list(Testcases) ->
  Testcases.



calculate_testcases_from_all(Forms) ->
  % if 'all' is specified in batiscaph_steps attribute
  % then transform should try to extract list of testcases from SUITE:all/0
  % function.

  % all/0 don't have any arguments, should have no guards
  % expected to have only one clause
  [{function,_,all,0,[{clause,_,[],[],Exprs}]}] = [F || {function,_,all,0,_} = F <- Forms],
  All = exec_ast_exprs(Exprs),
  GroupsToExpand = [Name || {group, Name} <- All],
  Testcases = [T || T <- All, is_atom(T)],
  case GroupsToExpand of
    [] -> Testcases;
    _ ->
      Testcases1 = calculate_testcases_from_groups(GroupsToExpand, Forms),
      lists:usort(Testcases ++ Testcases1)
  end.



calculate_testcases_from_groups(Names, Forms) ->
  [{function,_,groups,0,[{clause,_,[],[],Exprs}]}] = [F || {function,_,groups,0,_} = F <- Forms],
  Groups = exec_ast_exprs(Exprs),
  Map = group_testcases_map(Groups),
  lists:flatten(maps:values(maps:with(Names, Map))).



group_testcases_map(Groups) ->
  group_testcases_map(Groups, #{}).

group_testcases_map([], Acc) -> Acc;
group_testcases_map([{Name, _Opts, Testcases} | Groups], Acc) ->
  Testcases1 = [T || T <- Testcases, is_atom(T)],
  SubGroupNames = [G || {group, G} <- Testcases],
  SubGroups = maps:with(SubGroupNames, Acc),
  Testcases2 = lists:flatten(maps:values(SubGroups)),
  LeftSubGroups = SubGroupNames -- maps:keys(SubGroups),
  case LeftSubGroups of
    [] -> group_testcases_map(Groups, Acc#{Name => lists:usort(Testcases1 ++ Testcases2)});
    [_|_] ->
      % recursively finish the rest,
      % and then use freshly found groups to finish this one
      Acc1 = group_testcases_map(Groups, Acc),
      Testcases3 = [maps:get(G, Acc1) || G <- LeftSubGroups],
      Acc#{Name => lists:usort(Testcases1 ++ Testcases2 ++ Testcases3)}
  end.



exec_ast_exprs(Exprs) ->
  Bindings = erl_eval:new_bindings(),
  exec_ast_exprs(Exprs, Bindings).

exec_ast_exprs([E], Bindings) ->
  % for now no local functions execution is supported
  {value, Value, _Bindings1} = erl_eval:expr(E, Bindings),
  Value;

exec_ast_exprs([E | Exprs], Bindings) ->
  % for now no local functions execution is supported
  {value, _Value, Bindings1} = erl_eval:expr(E, Bindings),
  exec_ast_exprs(Exprs, Bindings1).



% this generates definition of local function
% that gives away any function in this module to anyone calling it
generate_local_fun_finder(EofLine, Forms) ->
  % generate random name of this function
  Name = <<"batiscaph_steps_local_fun_finder_", (integer_to_binary(rand:uniform(10000)))/binary>>,
  Name1 = binary_to_atom(Name, latin1),

  % all functions that presented in module
  FunArityList = [{Atom,Arity} || {function,_,Atom,Arity,_} <- Forms, Atom =/= Name1],

  Clauses = lists:map(fun ({Atom1, Arity1}) ->
    Args = [{atom,EofLine,Atom1},{integer,EofLine,Arity1}],
    Exprs = [{tuple,EofLine,[{atom,EofLine,ok},{'fun',EofLine,{function,Atom1,Arity1}}]}],
    % (Atom1, Arity1) -> {ok, fun Atom1/Arity1}
    {clause, EofLine, Args, [], Exprs}
  end, FunArityList),

  % (_,_) -> none
  NoneClause = {clause,EofLine,[{var,EofLine,'_'},{var,EofLine,'_'}],[],[{atom,EofLine,none}]},
  FunctionDefinition = {function,EofLine,Name1,2,Clauses ++ [NoneClause]},
  {ok, Name1, FunctionDefinition}.



wrap_functions([], _State) -> [];

% file attribute mean that following AST was taken from specific file
% for example via -include_lib(...)
% need to track current file, for proper source code extraction
wrap_functions([{attribute, _, file, {Path, _}} = F | Forms], #suite_trans{} = State) ->
  [F | wrap_functions(Forms, State#suite_trans{current_file = list_to_binary(Path)})];

wrap_functions([{function,_,Atom,Arity,_} = F | Forms], State)
when (Atom =:= init_per_suite andalso Arity == 1)
orelse (Atom =:= end_per_suite andalso Arity == 1)
orelse (Atom =:= init_per_group andalso Arity == 2)
orelse (Atom =:= end_per_group andalso Arity == 2)
orelse (Atom =:= init_per_testcase andalso Arity == 2)
orelse (Atom =:= end_per_testcase andalso Arity == 2) ->
  {ok, F1, State1} = wrap_one_function(F, State),
  [F1 | wrap_functions(Forms, State1)];

wrap_functions([{function,_,Atom,1,_} = F | Forms], #suite_trans{testcases = Testcases} = State) ->
  case lists:member(Atom, Testcases) of
    false -> [F | wrap_functions(Forms, State)];
    true ->
      {ok, F1, State1} = wrap_one_function(F, State),
      [F1 | wrap_functions(Forms, State1)]
  end;

wrap_functions([F | Forms], State) ->
  [F | wrap_functions(Forms, State)].


wrap_one_function({function, Line, Atom, Arity, Clauses} = F, #suite_trans{suite = Suite} = State) ->
  Context = get_context(F, Suite),
  {Clauses1, State1} = lists:foldl(fun (C, {Acc, State2}) ->
    {ok, C1, State3} = wrap_fun_clause(Context, C, State2),
    {[C1 | Acc], State3}
  end, {[], State}, Clauses),

  F1 = {function, Line, Atom, Arity, lists:reverse(Clauses1)},
  {ok, F1, State1}.



get_context({function, _, init_per_suite, 1, _}, Suite) -> {init_per_suite, Suite};
get_context({function, _, end_per_suite, 1, _}, Suite) -> {end_per_suite, Suite};
get_context({function, _, init_per_group, 2, _}, Suite) -> {init_per_group, Suite};
get_context({function, _, end_per_group, 2, _}, Suite) -> {end_per_group, Suite};
get_context({function, _, init_per_testcase, 2, _}, Suite) -> {init_per_testcase, Suite};
get_context({function, _, end_per_testcase, 2, _}, Suite) -> {end_per_testcase, Suite};
get_context({function, _, Atom, 1, _}, Suite) -> {testcase, Suite, Atom}.



gensym() ->
  list_to_atom("Batiscaph_gen_var" ++ integer_to_list(erlang:unique_integer([positive]))).

t_new_bindings() ->
  % erl_eval:new_bindings()
  {call,0,{remote,0,{atom,0,erl_eval},{atom,0,new_bindings}},[]}.

t_add_binding({var,_,VarAtom} = Var, Bindings) ->
  % erl_eval:add_binding('VarName',Var, @Bindings)
  {call,0,{remote,0,{atom,0,erl_eval},{atom,0,add_binding}},[{atom,0,VarAtom},Var,Bindings]}.



t_cons([]) -> {nil,0};
t_cons([E | Rest]) -> {cons,0,E,t_cons(Rest)}.



wrap_vars_for_bindings(Vars) ->
  {Vars1, Bindings} = lists:foldl(fun
    ({var,_,'_'}, {Vars1, Bindings}) ->
      % because var was ignored, do not add it to bindings
      % rename it, to be able to pass it to batiscaph_steps:exec_steps as arg 
      Var1 = {var,0,gensym()},
      {[Var1 | Vars1], Bindings};

    ({var,_,_} = V, {Vars1, Bindings}) ->
      % variable has a name, add it to bindings
      % will be passed to batiscaph_steps:exec_steps as it is
      {[V | Vars1], t_add_binding(V, Bindings)};

    (Other, {Vars1, Bindings}) ->
      % looks like it's not a variable, but a pattern or literal
      % should keep it like that
      {[Other | Vars1], Bindings}
  end, {[], t_new_bindings()}, Vars),
  {ok, lists:reverse(Vars1), Bindings}.



% just quote all expressions, and pass them to batiscaph_shell
% also create new bindings, add Config arg
wrap_fun_clause(Context, {clause,Line,Vars,Guards,Exprs}, #suite_trans{local_finder_name = LocalFinderName} = State) ->
  {ok, Vars1, Bindings} = wrap_vars_for_bindings(Vars),
  Context1 = erl_syntax:revert(erl_syntax:abstract(Context)),
  {ok, Lines, State1} = get_source_lines(Line, last_line_in_forms(Exprs, Line), State),
  Lines1 = erl_syntax:revert(erl_syntax:abstract(Lines)),

  QuotedTree = erl_syntax:revert(erl_syntax:abstract(Exprs)),
  % erl_syntax:revert(QuotedTree)
  RevertedQuoted = {call,0,{remote,0,{atom,0,erl_syntax},{atom,0,revert_forms}},[QuotedTree]},
  % fun local_fun_handler/2
  LocalFinder = {'fun',0,{function,LocalFinderName,2}},
  % batiscaph_shell:exec_steps({testcase, suite_name, testcase_name}, Lines, [Config], erl_eval:add_binding('VarName',{Var},erl_eval:new_bindings()), fun local_fun_handler/2, Forms),
  Exprs1 = [{call,0,{remote,0,{atom,0,batiscaph_steps},{atom,0,exec_steps}}, [Context1, t_cons(Vars1), Lines1, Bindings, LocalFinder, RevertedQuoted]}],

  C = {clause,0,Vars1,Guards,Exprs1},
  {ok, C, State1}.



last_line_in_forms([], Line) -> Line;
last_line_in_forms(Forms, Line) ->
  % second element in tuple is always a line number
  Last = lists:last(Forms),
  lists:max([Line] ++ lists:flatten(get_subexpr_lines(Last))).



% take second element from all tuples,
% repeat recursively for all nested tuples
get_subexpr_lines(Form)
when is_tuple(Form) andalso is_atom(element(1, Form)) andalso is_integer(element(2, Form)) ->
  Line = element(2, Form),
  SubExprLines = lists:map(fun (I) ->
    get_subexpr_lines(element(I, Form))
  end, lists:seq(3, tuple_size(Form))),
  [Line | SubExprLines];

get_subexpr_lines(Form) when is_list(Form) -> [get_subexpr_lines(F) || F <- Form];
get_subexpr_lines(_Form) -> [].



ensure_file_added(Path, #suite_trans{files = Files} = State) ->
  case maps:get(Path, Files, undefined) of
    #source_file{} -> {ok, State};
    undefined ->
      {ok, Binary} = file:read_file(Path),
      Files1 = maps:put(Path, #source_file{left_content = Binary, path = Path}, Files),
      {ok, State#suite_trans{files = Files1}}
  end.



get_source_lines(FromLine, ToLine, #suite_trans{current_file = Path} = State)
when FromLine > 0 andalso ToLine > 0 ->
  {ok, #suite_trans{files = Files} = State1} = ensure_file_added(Path, State),
  #source_file{} = File = maps:get(Path, Files),

  {ok, Lines, File1} = get_source_lines1(FromLine, ToLine, File),
  Files1 = maps:put(Path, File1, Files),
  {ok, Lines, State1#suite_trans{files = Files1}}.

get_source_lines1(FromLine, ToLine, #source_file{prev_lines = Lines} = File)
when length(Lines) >= ToLine ->
  % actually this total reverse might be slow on big files
  % better to take sublist first and reverse it
  % but current code is simplier to understand
  Lines1 = lists:sublist(lists:reverse(Lines), FromLine, (ToLine - FromLine) + 1),
  Pairs = lists:zip(lists:seq(FromLine,ToLine), Lines1),
  {ok, Pairs, File};
get_source_lines1(FromLine, ToLine, #source_file{prev_lines = Lines, left_content = Content} = File) ->
  File1 = case binary:split(Content, [<<"\r\n">>,<<"\n">>]) of
    [Line, Content1] -> File#source_file{prev_lines = [Line | Lines], left_content = Content1};
    [Line] -> File#source_file{prev_lines = [Line | Lines], left_content = undefined}
  end,
  get_source_lines1(FromLine, ToLine, File1).

