-module(dt).
-export([somebool/1]).

somebool([]) -> false;
somebool([X|Xs]) -> 
case is_boolean(X) of 	
		true -> true;
		false -> somebool(Xs)
end.