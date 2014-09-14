-module(haar).
-export([encode/1, decode/1]).
-include_lib("eunit/include/eunit.hrl").

encode(L) when is_list(L) ->
	encode(L, math:log(length(L)) / math:log(2)).

encode(L, Steps) when Steps > 0 ->
	{A, D} = encode_once(L, {[], []}),
	encode(A ++ D, Steps - 1);
encode(L, _) ->
	L.

encode_once([], {Averages, Differences}) -> {lists:reverse(Averages), lists:reverse(Differences)};
encode_once([A, B | T], {Averages, Differences}) ->
	Average = (A + B) / 2,
	Difference = A - Average,
	encode_once(T, {[Average | Averages], [Difference | Differences]}).

encode_test_() ->
	[?_assertEqual([6.5, 1.5], encode([8, 5])),
	 ?_assertEqual([5.75, 1.75, 0.75, -0.25], encode([8, 5, 7, 3]))
	].


decode(L) when is_list(L) ->
	decode(L, math:log(length(L)) / math:log(2)).

decode(L, Steps) when Steps > 0 ->
	N = length(L),
	decode(decode_once(lists:split(trunc(N/2), L), []), Steps - 1);
decode(L, _) ->
	L.

decode_once({[A | Atail], [D | Dtail]}, Acc) ->
	decode_once({Atail, Dtail}, [A - D, A + D | Acc]);
decode_once({[], []}, Acc) -> lists:reverse(Acc).


decode_test_() ->
	[?_assertEqual([8.0, 5.0], decode([6.5, 1.5])),
	?_assertEqual([8.0, 5.0, 7.0, 3.0], decode([5.75, 1.75, 0.75, -0.25]))]
	.

chain_test_() ->
	[?_assertEqual([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0], decode(encode([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0])))].
