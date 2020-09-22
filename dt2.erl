-module(dt2).
-export([remove/2]).

remove(Y, []) -> [];
remove(Y, [X|Xs]) ->
  case Y == X of
    true -> remove(Y, Xs);
    false -> [X| remove(Y, Xs)]
  end.
