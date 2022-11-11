-module(test_frappe).

-export([test_all/0, test_everything/0]).
-export([]). % Remember to export the other functions from Q2.2


% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_frappe.

test_all() ->
  ok.

test_everything() ->
  test_all().
