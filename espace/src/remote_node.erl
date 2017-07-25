-module(remote_node).
-export([load_local_module/2]).



% currently object code just compiled from local source files
%
% in future it may require to have different object files for different versions of Erlang
% also some production systems may have very special version of erlang and require modules to be compiled remotely
load_local_module(Node, Module) ->
  {Module, Binary, Path} = code:get_object_code(Module),
  {module, Module} = rpc:call(Node, code, load_binary, [Module, Path, Binary]),
  ok.
