-module(frappe).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called frappe.

% Export at least the API:
-export([fresh/1,
         set/4,
         read/2,
         insert/4,
         update/4,
         upsert/3,
         stable/3,
         all_items/1,
         stop/1
        ]).

% You may have other exports as well
-export([]).



fresh(_) ->
  not_implemented.

set(_, _, _, _) ->
  not_implemented.

read(_, _) ->
  not_implemented.

insert(_, _, _, _) ->
  not_implemented.

update(_, _, _, _) ->
  not_implemented.

upsert(_, _, _) ->
  not_implemented.

stable(_, _, _) ->
  not_implemented.

all_items(_) ->
  not_implemented.

stop(_) ->
  not_implemented.
