-module(async_new).

-export([new/2, wait/1, poll/1, add/1, findD/0, create_aid/1, get_all/0]).



new(Fun, Arg) -> 
    Pid =  whereis(aidprocess),
    if 
        Pid == undefined ->
            NewPid = start(),
            register(aidprocess, NewPid);
        true ->
            whereis(aidprocess) 
    end,
    request_reply(whereis(aidprocess), {new_fun, Fun, Arg}).

get_all() ->
    request_reply(whereis(aidprocess), {get_all}). 

wait(Aid) -> nope.
poll(Aid) -> nope.

start() -> spawn(fun() -> loop([]) end).

handle_call(Request, State) ->
    case Request of
        % {new_fun, Fun, Arg} -> worker_process(Fun, Arg, State);
        {wait_fun, Aid} -> wait(Aid);
        {poll_fun, Aid} -> poll(Aid)
    end.

worker_process(Fun, Arg, Aids, Aid) ->
    Me = self(),
    process_flag(trap_exit, true),
    Worker = spawn_link(fun() ->
                        Fun_Res = Fun(Arg),
                        Me ! {self(), Fun_Res}
                        end),
    NewAids = receive
        {Worker, Fun_Res} -> 
            case [X || X <- Aids, maps:get("aid", X) == Aid] of
                [] -> connot_find;
                [Value] -> Value#{"res" := {ok,Fun_Res}}
            end,
            Aids;
        {'EXIT', Worker, Reason} ->
            case [X || X <- Aids, maps:get("aid", X) == Aid] of
                [] -> connot_find;
                [Value] -> Value#{"res" := {error, Reason}}
            end,
            Aids
    end,
    io:fwrite(NewAids),
    loop(NewAids).

request_reply(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Response} -> Response
    end.

loop(Aids) ->
    receive
        {From, Ref, {new_fun, Fun, Arg}} ->
            % io:fwrite(Request),
            Id = create_aid(Aids),
            Item = #{"aid" => Id, "res" => nothing},
            NewAids = [Item | Aids],
            From ! {Ref, Id},
            worker_process(Fun, Arg, NewAids, Ref),
            loop(NewAids);
        {From, Ref, {poll_fun, Aid}} ->
            case [X || X <- Aids, maps:get("aid", X) == Aid] of
                [] -> connot_find;
                [Value] -> Value
            end;
        {From, Ref, {get_all}} ->
            From ! {Ref, Aids},
            loop(Aids)
    end.


create_aid([]) -> 1;
create_aid([H|_]) -> maps:get("aid", H) + 1.


add(A) -> A + 1.


findD() ->
    D = [#{"aid"=>1, "res"=>1}, #{"aid"=>2, "res"=>2}],
    case [X || X <- D, maps:get("aid", X) == 1] of
        [] -> nothing;
        [Value] -> Value
    end.
    % C = #{"aid" => 1, "res"=>1},
    % maps:find, Arg2)

