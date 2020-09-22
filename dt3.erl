-module(dt3).
-export([gtr/2]).

gtr(Y, []) -> [];
gtr(Y, [X|Xs]) ->
  case X>Y of
    true -> [X|gtr(Y,Xs)];
    false -> gtr(Y,Xs)
  end.
