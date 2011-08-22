-module(map).

-export([exists/2, get/2, keys/1, new/1, put/3,
	 remove/2, values/1]).

new(Name) -> ets:new(Name, []).

put(Ets, Key, Value) ->
    true = ets:insert(Ets, [{Key, Value}]).

get(Ets, Key) ->
    LookupResult = ets:lookup(Ets, Key),
    case LookupResult of
      [H] -> {_K, V} = H, V;
      [H | _T] -> {_K, V} = H, V;
      [] -> undefined
    end.

remove(Ets, Key) ->
    case get(Ets, Key) of
      undefined -> undefined;
      Other -> ets:delete(Ets, Key), Other
    end.

keys(Ets) ->
    ets:foldl(fun ({K, _V}, Acc) -> [K] ++ Acc end, [],
	      Ets).

values(Ets) ->
    ets:foldl(fun ({_K, V}, Acc) -> [V] ++ Acc end, [],
	      Ets).

exists(Ets, Key) ->
    case get(Ets, Key) of
      undefined -> false;
      _ -> true
    end.
