-module(usage).

-export([small_capacity/0, small_upsert/0]).


small_capacity() ->
  {ok, FS} = frappe:fresh(5),
  ok = frappe:set(FS, key1, 42, 2),
  io:format("When looking up ~w I got ~w, and I expected {ok, 42}~n",
            [key1, frappe:read(FS, key1)]),
  ok = frappe:set(FS, key2, 69, 2),
  {ok, 42} = frappe:read(FS, key1),
  ok = frappe:set(FS, big, 300, 3),
  io:format("When looking up ~w I got ~w, and I expected {ok, 42}~n",
            [key1, frappe:read(FS, key1)]),
  io:format("When looking up ~w I got ~w, and I expected nothing~n",
            [key2, frappe:read(FS, key2)]),
  frappe:stop(FS).


small_upsert() ->
  {ok, FS} = frappe:fresh(5),
  ok = frappe:set(FS, key1, [a,b], 2),
  io:format("When looking up ~w I got ~w, and I expected {ok, [a,b]}~n",
            [key1, frappe:read(FS, key1)]),
  ok = frappe:set(FS, key2, [a,b], 2),
  ok = frappe:upsert(FS, key2,
                     fun({existing, Val}) ->
                         New = Val ++ [c,d],
                         {new_value, New, length(New)}
                     end),
  io:format("When looking up ~w I got ~w, and I expected nothing~n",
            [key1, frappe:read(FS, key1)]),
  frappe:stop(FS).
