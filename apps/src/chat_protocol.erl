-module(chat_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, [Server | _Opts]) ->
  ok = ranch:accept_ack(Ref),
  Server ! {connected, Socket},
  loop(Server, Socket, Transport, "").

loop(Server, Socket, Transport, Acc) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Packet} ->
      LPacket = binary_to_list(Packet),
      case lists:reverse(LPacket) of
        [$\n | _Data] ->
          Commands = string:tokens(Acc ++ LPacket, "\n"),
          lists:map(fun(C)-> parse_message(Server, Transport, Socket, C ++ "\n") end, Commands),
          loop(Server, Socket, Transport, "");
        _ -> loop(Server, Socket, Transport, Acc ++ LPacket)
      end;
    _ ->
      ok = Transport:close(Socket)
  end.


parse_message(Server, Transport, Socket, Msg) ->
  case strip_eol(Msg) of
    "\\help" -> Transport:send(Socket, "available commands:\r\n help\r\n greetme\r\n name YOUR_NAME\r\n");
    "\\greetme" -> Transport:send(Socket, "hi\r\n");
    "\\name " ++ Name ->
      Server ! {name, Socket, Name},
      Transport:send(Socket, "your name is: " ++ Name ++ "\r\n");
    "\\" ++ Cmd -> Transport:send(Socket, "invalid command: " ++ Cmd ++ "\r\n");
    _ -> Server ! {msg, Transport, Socket, Msg}
  end.

strip_eol(Msg) ->
  string:strip(string:strip(Msg, both, $\n), both, $\r).
