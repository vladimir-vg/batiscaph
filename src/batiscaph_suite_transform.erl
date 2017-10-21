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
  testcases :: [atom()],
  local_finder_name :: atom(),
  current_file :: binary(),
  files = #{} :: #{Path :: binary() => #source_file{}}
}).



parse_transform(Forms, _Options) ->
  case get_steps_attr_value(Forms) of
    undefined -> Forms;
    {ok, Testcases} ->
      {eof, EofLine} = lists:last(Forms),
      % generate secret function that provides access to local functions in suite
      % later append it at the end of file
      % this required for proper step-by-step execution by erl_eval
      {ok, LocalFinderName, LocalFunHandler} = generate_local_fun_finder(EofLine, Forms),
      % io:format("forms:~n~p~n", [Forms]),
      State = #suite_trans{testcases = Testcases, local_finder_name = LocalFinderName},
      case wrap_functions(Forms, State) of
        Forms -> Forms; % no changes made, no need for local fun handler
        Forms1 when Forms1 =/= Forms ->
          % insert definiton of local fun handler at the end of the file
          [{eof, EofLine} | Rest] = lists:reverse(Forms1),
          lists:reverse([{eof, EofLine}, LocalFunHandler | Rest])
      end
  end.

get_steps_attr_value([]) -> undefined;
get_steps_attr_value([{attribute,_Line,batiscaph_steps,Value} | _Forms]) -> {ok, Value};
get_steps_attr_value([_ | Forms]) -> get_steps_attr_value(Forms).



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
wrap_functions([{attribute, _, file, {Path, _}} = F | Forms], #suite_trans{} = State) ->
  [F | wrap_functions(Forms, State#suite_trans{current_file = list_to_binary(Path)})];
wrap_functions([{function,_,Atom,1,_} = F | Forms], #suite_trans{testcases = Testcases} = State) ->
  case lists:member(Atom, Testcases) of
    false -> [F | wrap_functions(Forms, State)];
    true ->
      {ok, F1, State1} = wrap_one_function(F, State),
      [F1 | wrap_functions(Forms, State1)]
  end;
wrap_functions([F | Forms], State) ->
  [F | wrap_functions(Forms, State)].


wrap_one_function({function, Line, Atom, 1, Clauses}, #suite_trans{} = State) ->
  {Clauses1, State1} = lists:foldl(fun (C, {Acc, State2}) ->
    {ok, C1, State3} = wrap_fun_clause(Atom, C, State2),
    {[C1 | Acc], State3}
  end, {[], State}, Clauses),
  % Clauses1 = [ || C <- Clauses],
  F = {function, Line, Atom, 1, lists:reverse(Clauses1)},
  {ok, F, State1}.



% just quote all expressions, and pass them to batiscaph_shell
% also create new bindings, add Config arg
wrap_fun_clause(FuncAtom, {clause,Line,[Var],Guards,Exprs}, #suite_trans{local_finder_name = LocalFinderName} = State) ->
  {ok, Lines, State1} = get_source_lines(Line, last_line_in_forms(Exprs, Line), State),

  QuotedTree = erl_syntax:revert(erl_syntax:meta(erl_syntax:form_list(Exprs))),
  Lines1 = erl_syntax:revert(erl_syntax:abstract(Lines)),
  % erl_eval:new_bindings()
  NewBindings = {call,Line,{remote,Line,{atom,Line,erl_eval},{atom,Line,new_bindings}},[]},
  Var1 = {var,Line,'Config'}, % use this name for config var, pass it to steps
  Bindings = case Var of
    {var,_,'_'} -> NewBindings;
    {var,_,VarName} ->
      % erl_eval:add_binding('VarName',Var,erl_eval:new_bindings())
      {call,Line,{remote,Line,{atom,Line,erl_eval},{atom,Line,add_binding}},[{atom,Line,VarName},Var1,NewBindings]}
  end,
  % erl_syntax:revert(QuotedTree)
  RevertedQuoted = {call,Line,{remote,Line,{atom,Line,erl_syntax},{atom,Line,revert_forms}},[QuotedTree]},
  % fun local_fun_handler/2
  LocalFinder = {'fun',Line,{function,LocalFinderName,2}},
  % batiscaph_shell:exec_testcase(testcase_name, Lines, Config, erl_eval:add_binding('VarName',{Var},erl_eval:new_bindings()), fun local_fun_handler/2, Forms),
  Exprs1 = [{call,Line,{remote,Line,{atom,Line,batiscaph_steps},{atom,Line,exec_testcase}}, [{atom,Line,FuncAtom}, Lines1, Var1, Bindings, LocalFinder, RevertedQuoted]}],

  C = {clause,Line,[Var1],Guards,Exprs1},
  {ok, C, State1}.



last_line_in_forms([], Line) -> Line;
last_line_in_forms(Forms, _Line) -> element(2, lists:last(Forms)). % second element in tuple is always line number



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

