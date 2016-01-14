-module(chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Server = spawn(fun() -> server_loop(maps:new()) end),
    {ok, _} = ranch:start_listener(chat, 1, ranch_tcp, [{port, 6667}], chat_protocol, [Server]),
    chat_sup:start_link().

stop(_State) ->
    ok.

server_loop(Connections) ->
    receive
        {connected, Socket} -> server_loop(maps:put(Socket, "", Connections));
        {disconnected, Socket} -> server_loop(maps:remove(Socket, Connections));
        {name, Socket, Name} -> server_loop(maps:update(Socket, Name ++ ": ", Connections));
        {msg, Transport, Receiver, Data} ->
            sendout_message(Transport, maps:remove(Receiver, Connections), maps:get(Receiver, Connections) ++ Data),
            server_loop(Connections);
        {close} -> ok
    end.

sendout_message(Transport, Connections, Msg) ->
    List = maps:keys(Connections),
    lists:map(fun(C) -> Transport:send(C, Msg) end, List).

