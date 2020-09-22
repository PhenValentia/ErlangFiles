-module(foo).
-export([foo/1, max/2, maxThree/3, howManyEqual/3, perimeter/1, fib/1, fib2/1, test/1, sum/1, sumRange/2, cut/1, mult/1, merge/2]).

foo(0) -> 37;
foo(N) -> 42.


merge([], []) -> [];
merge([X|Xs], []) -> [X|Xs];
merge([], [X|Xs]) -> [X|Xs];
merge([X|Xs], [Y|Ys]) ->
if
		X < Y -> [X] ++ merge(Xs, [Y|Ys]);
		true -> [Y] ++ merge([X|Xs], Ys)
end.



mult([]) -> 1;
mult([X|Xs]) -> X*mult(Xs).


maxThree(X, Y, Z) ->
if 
	X > Y andalso X > Z -> X;
	Y > X andalso Y > Z -> Y;
	true -> Z
end.

max(X, Y) -> 
if X >= Y -> X;
true -> Y
end.

howManyEqual(X, Y, Z) ->
if 
	X == Y orelse X == Z -> A = 1;
	true -> A = 0
end,
if 
	X == Y orelse Y == Z -> B = 1;
	true -> B = 0
end,
if 
	Z == Y orelse X == Z -> C = 1;
	true -> C = 0
end,
T = A+B+C,
T.

perimeter({circle, R}) ->
	2*R*math:pi();
perimeter({rectangle, H, W}) ->
	2*(H+W).
	
fib(0) ->
	0;
fib(1) ->
	1;
fib(N) ->
	fib(N-1) + fib(N-2).

fib2(0) ->	0;
fib2(1) ->	1;	
fib2(N) ->
	{fib(N-1) + fib(N-2), fib(N) + fib(N-1)}.
	
test(N) -> {N, N}.

sum(1) -> 1;
sum(N) -> N + sum(N-1). 

sumRange(M, N) -> 
if
	M =/= N -> N + sumRange(M, N-1);
	true -> M
end.

cut(N) ->
	(N*N+N+2)/2.