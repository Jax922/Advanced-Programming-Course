-module(async).

-export([new/2, wait/1, poll/1]).

%%=========================== export functions ===========================%%
new(Fun, Arg) -> 
   spawn(fun() -> worker_process(Fun, Arg) end).

wait(Aid) -> 
    case poll(Aid) of
        nothing -> 
            timer:sleep(50),
            wait(Aid);
        {ok, Res} -> Res;
        {exception, Ex} -> throw(Ex);
        {error, Reason} -> throw(Reason)
    end.

poll(Aid) -> 
    request_reply(Aid, {poll_fun}).

%%=========================== Async Server ===========================%%
% create a Worker to get Fun(Arg) value
worker_process(Fun, Arg) ->
    Me = self(),
    % process_flag(trap_exit, true),
    spawn_link(fun() ->
        Fun_Res = try
           {ok, Fun(Arg)}
        catch
            Ex -> {exception, Ex}
        end,
        Me ! {self(), {done, Fun_Res}}
        end),
    loop({init, nothing}).

% send msg to a Aid, and receive msg from a Aid
request_reply(Aid, Request) ->
    Ref = make_ref(),
    Aid ! {self(), Ref, Request},
    receive
        {Ref, Response} -> Response
    end.

% loop is the Async Server, State is designed as follows.
% State = {init/done, Res}
% Res = nothing/{ok, Fun_Res}/{exception, Ex}/{EXIT, Reason}
loop(State) ->
    receive 
        {_, {done, Fun_Res}} ->
            io:fwrite("receive msg~p~n", [Fun_Res]),
            loop({done, Fun_Res});
        % {'EXIT', _, Reason} ->
        %     io:fwrite("receive error msg~p~n", [Reason]),
        %     loop({done,{error, Reason}}); 
        {From, Ref, {poll_fun}} ->
            case State of
                {init, nothing} ->  From ! {Ref, nothing};
                {done, Res} -> From ! {Ref, Res}
            end,
            loop(State)
    end.



