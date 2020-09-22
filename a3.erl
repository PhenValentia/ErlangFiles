-module(a3).
-compile(export_all).

  parFind(_, [], _) -> false;
  parFind(X, [X], Parent) -> Parent ! true;
  parFind(_, [_], Parent) -> Parent ! false;
  parFind(X, Xs, Parent) ->
      {L, R} = lists:split(length(Xs) div 2, Xs),
      spawn(?MODULE, parFind, [L, X, self()]),
      spawn(?MODULE, parFind, [R, X, self()]),
      receive
          Left ->
              receive
                  Right -> Parent!{Left or Right}
              end
      end.

  find(X, Xs) -> spawn(?MODULE, parFind, [X, Xs, self()]).



% Question 2 Below

server() -> 
    receive 
        {inc, P, X} -> P!{result, (X+1)}, server(); 
        {dec, P, X} -> P!{result, (X-1)}, server();
        {stop, P} -> P!{result, ack} 
    end.

client(M, Pid) ->
    Pid!{M, self()},
    receive 
		{result, U} ->
		io:fwrite("Stopped: ~p~n",[U])
	end.
client(M, Pid, X) ->
    Pid!{M, self(), X},
    receive 
		{result, U} ->
		io:fwrite("Result: ~p~n",[U])
	end.

test() ->
	P = spawn(?MODULE, server,[]),
	spawn(?MODULE, client,[inc, P, 5]),
    spawn(?MODULE, client,[dec, P, 5]),
    timer:sleep(round(rand:uniform()*1000)),
    spawn(?MODULE, client,[stop, P]).