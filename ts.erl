-module(ts).
-compile(export_all).

worker(P, D1, D2) ->
	P!{result,(D1*D2)}.
	
program({X, Y}, {A, B}) ->
	spawn(?MODULE, worker, [self(), A, B]),
	spawn(?MODULE, worker, [self(), X, Y]),
	receive {result, C} ->
		receive {result, D} ->
			D-C
		end
	end.

server() ->
	receive
		{helo, P} -> 
			receive {X, P} -> 
				receive {Y, P} -> 
					P!{result,(X+Y)} , server()
				end 
			end;
		stop -> []
	end.
	
client(Pid, X, Y) ->
	Test1 = X,
	Test2 = Y,
	
	Pid!{helo,self()},
	Pid!{Test1,self()},
	Pid!{Test2,self()},
	receive 
		{result, U} ->
		io:fwrite("Result: ~p~n",[U])
	end.
	
test() ->
	Tid = spawn(?MODULE, server,[]),
	spawn(?MODULE, client,[Tid, 2, 4]),
	spawn(?MODULE, client,[Tid, 7, 4]).
	