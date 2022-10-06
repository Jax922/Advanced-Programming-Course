-module(async).

-export([new/2, wait/1, poll/1]).


% return Aid
new(Fun, Arg) -> 
    Aid = spawn(fun() -> loop({init, nothing}) end),
    request_reply(Aid, {new_fun, Fun, Arg}),
    Aid.

wait(Aid) -> 
    request_reply(Aid, {wait_fun, Aid}).

poll(Aid) -> 
    request_reply(Aid, {poll_fun}).

% create a Worker to get Fun(Arg) value
worker_process(Fun, Arg) ->
    Me = self(),
    process_flag(trap_exit, true),
    Worker = spawn_link(fun() ->
                        Fun_Res = try
                           {ok, Fun(Arg)}
                        catch
                            Ex -> {exception, Ex}
                        end,
                        Me ! {self(),{done, Fun_Res}}
                        end),
    receive
        {Worker, {done, Fun_Res}} -> 
            case Fun_Res of
                {ok, Res} -> {done, {ok, Res}};
                {exception, E} -> {done, {exception, E}}
            end;
        {'EXIT', Worker, Reason} ->
                {done, {error, Reason}}
    end.

% handle_call always report error, so comment it.
% handle_call(Request) ->
%     case Request of
%         {new_fun, Fun, Arg} -> worker_process(Fun, Arg);
%         {wait_fun, Aid} -> wait(Aid);
%         {poll_fun, Aid} -> poll(Aid)
%     end.

% send msg to a Aid, and receive msg from a Aid
request_reply(Aid, Request) ->
    Ref = make_ref(),
    Aid ! {self(), Ref, Request},
    receive
        {Ref, Response} -> Response
    end.

% State = {init/done, res}
% res = nothing/{ok, Fun_Res}/{exception, Ex}/{EXIT, Reason}
loop(State) ->
    receive 
    % {From, Ref, Request} ->
    %     % io:fwrite(Request),
    %     {NewState, Res} = handle_call(Request),
    %     From ! {Ref, Res},
    %     loop(NewState)
        {From, Ref, {new_fun, Fun, Arg}} ->
            NewState = worker_process(Fun, Arg),
            From ! {Ref, NewState},
            loop(NewState);
        {From, Ref, {poll_fun}} ->
            case State of
                {init, nothing} ->  From ! {Ref, nothing};
                {done, Res} -> From ! {Ref, Res}
            end;
        {From, Ref, {wait_fun, Aid}} ->
            case State of
                {init, nothing} -> 
                    time:sleep(300), 
                    wait(Aid);
                {done, {ok, Res}} -> From ! {Ref, Res}; 
                {done, {exception, Ex}} -> throw(Ex);
                {done, {error, Reason}} -> throw(Reason)
            end
    end.




