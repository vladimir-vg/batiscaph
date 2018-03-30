-module(vision_util).
-export([binary_to_hex/1]).



% stolen from here:
% https://stackoverflow.com/a/29819282/614661
binary_to_hex(Bin) when is_binary(Bin) ->
  << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.
