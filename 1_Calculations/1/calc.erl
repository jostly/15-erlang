-module(calc).
-export([calc/1]).

calc(L) when is_list(L) ->
	apply_calc(string:tokens(L, " ")).

apply_calc(["sum" | T]) ->
	lists:foldl(fun(X, Sum) -> read(X) + Sum end, 0, T);
apply_calc(["product" | T]) ->
	lists:foldl(fun(X, Sum) -> read(X) * Sum end, 1, T);
apply_calc(["mean" | T]) ->
	{Sum, Count} = lists:foldl(fun(X, {Sum, Count}) -> {read(X) + Sum, Count + 1} end, {0, 0}, T),
	Sum / Count;
apply_calc(["sqrt" | T]) ->
	lists:map(fun my_sqrt/1, T).

my_sqrt(N) when is_list(N) ->
	my_sqrt(read(N));
my_sqrt(N) when N >= 0 ->
	math:sqrt(N);
my_sqrt(N) when N < 0 ->
	{0, math:sqrt(-N)}. % A 2-tuple here implies a complex number with {Re, Im}

read(N) ->
	case string:to_float(N) of
		{error, no_float} -> list_to_integer(N);
		{F, _} -> F
	end.