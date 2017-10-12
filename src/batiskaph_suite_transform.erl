-module(batiskaph_suite_transform).
-export([parse_transform/2]).



%%% This parse_transform takes testcases with -batiskaph_steps(). attribute
%%% and replaces their body with batiskaph_shell call, with AST.
%%% Basically just quotes body of the testcase for future step by step execution
%%% by batiskaph_shell.



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
      case wrap_functions(Forms, LocalFinderName, Testcases) of
        Forms -> Forms; % no changes made, no need for local fun handler
        Forms1 when Forms1 =/= Forms ->
          % insert definiton of local fun handler at the end of the file
          [{eof, EofLine} | Rest] = lists:reverse(Forms1),
          lists:reverse([{eof, EofLine}, LocalFunHandler | Rest])
      end
  end.

get_steps_attr_value([]) -> undefined;
get_steps_attr_value([{attribute,_Line,batiskaph_steps,Value} | _Forms]) -> {ok, Value};
get_steps_attr_value([_ | Forms]) -> get_steps_attr_value(Forms).



% this generates definition of local function
% that gives away any function in this module to anyone calling it
generate_local_fun_finder(EofLine, Forms) ->
  % generate random name of this function
  Name = <<"batiskaph_steps_local_fun_finder_", (integer_to_binary(rand:uniform(10000)))/binary>>,
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



wrap_functions([], _LocalFinderName, _Testcases) -> [];
wrap_functions([{function,_,Atom,1,_} = F | Forms], LocalFinderName, Testcases) ->
  case lists:member(Atom, Testcases) of
    false -> [F | wrap_functions(Forms, LocalFinderName, Testcases)];
    true -> [wrap_one_function(F, LocalFinderName) | wrap_functions(Forms, LocalFinderName, Testcases)]
  end;
wrap_functions([F | Forms], LocalFinderName, Testcases) ->
  [F | wrap_functions(Forms, LocalFinderName, Testcases)].


wrap_one_function({function, Line, Atom, 1, Clauses}, LocalFinderName) ->
  Clauses1 = [wrap_fun_clause(C, LocalFinderName) || C <- Clauses],
  {function, Line, Atom, 1, Clauses1}.



% just quote all expressions, and pass them to batiskaph_shell
% also create new bindings, add Config arg
wrap_fun_clause({clause,Line,[Var],Guards,Exprs}, LocalFinderName) ->
  QuotedTree = erl_syntax:revert(erl_syntax:meta(erl_syntax:form_list(Exprs))),
  % erl_eval:new_bindings()
  NewBindings = {call,Line,{remote,Line,{atom,Line,erl_eval},{atom,Line,new_bindings}},[]},
  Bindings = case Var of
    {var,_,'_'} -> NewBindings;
    {var,_,VarName} ->
      % erl_eval:add_binding('VarName',Var,erl_eval:new_bindings())
      {call,Line,{remote,Line,{atom,Line,erl_eval},{atom,Line,add_binding}},[{atom,Line,VarName},Var,NewBindings]}
  end,
  % erl_syntax:revert(QuotedTree)
  RevertedQuoted = {call,Line,{remote,Line,{atom,Line,erl_syntax},{atom,Line,revert_forms}},[QuotedTree]},
  % fun local_fun_handler/2
  LocalFinder = {'fun',Line,{function,LocalFinderName,2}},
  % batiskaph_shell:steps_exec(erl_eval:add_binding('VarName',{Var},erl_eval:new_bindings()), fun local_fun_handler/2, Forms),
  Exprs1 = [{call,Line,{remote,Line,{atom,Line,batiskaph_shell},{atom,Line,steps_exec}}, [Bindings, LocalFinder, RevertedQuoted]}],
  {clause,Line,[Var],Guards,Exprs1}.

