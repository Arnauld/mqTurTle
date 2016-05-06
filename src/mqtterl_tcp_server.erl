%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. avr. 2016 19:18
%%%-------------------------------------------------------------------
-module(mqtterl_tcp_server).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, stop/0]).

-define(SERVER, ?MODULE).

-record(state, {listener, handler}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port, Handler = {InitFn, HandlerFn}) when is_integer(Port), is_function(InitFn, 0), is_function(HandlerFn, 3) ->
  Opts = [binary, {active, false}, {keepalive, true}, {backlog, 30}],
  case gen_tcp:listen(Port, Opts) of
    {ok, ListenSocket} ->
      State = #state{listener = ListenSocket, handler = Handler},
      Pid = spawn_link(fun() ->
        io:format("Ready to accept connection on port ~p~n", [Port]),
        accept(State)
      end),
      {ok, Pid};

    {error, Reason} ->
      {stop, Reason}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> ok).
stop() ->
  ok.

%%%===================================================================
%%% INTERNAL FUNCTIONS
%%%===================================================================

accept(State) ->
  #state{listener = ListenSocket, handler = Handler} = State,
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      Pid = spawn_link(fun() ->
        io:format("Connection accepted ~p~n", [Socket]),
        listen_loop_(Socket, Handler)
      end),
      gen_tcp:controlling_process(Socket, Pid),
      Pid ! ack,
      accept(State);

    Error ->
      exit(Error)
  end.

listen_loop_(Socket, {HandlerInitFn, HandlerFn}) ->
  %% make sure to acknowledge owner rights transmission finished
  receive ack -> ok end,

  ready_to_receive_tcp_data(Socket),
  listen_loop(Socket, HandlerFn, HandlerInitFn()).

ready_to_receive_tcp_data(Socket) ->
  %% set soscket options to receive messages directly into itself
  inet:setopts(Socket, [{active, once}]).

listen_loop(Socket, HandlerFn, State) ->
  receive
    {tcp, Socket, NewData} ->
      io:format("tcp::Got packet: ~p~n", [NewData]),
      case HandlerFn(Socket, State, NewData) of
        {ok, NewState} ->
          ready_to_receive_tcp_data(Socket),
          listen_loop(Socket, HandlerFn, NewState);

        {disconnect, _} ->
          gen_tcp:close(Socket);

        What ->
          io:format("tcp::unknown message from handler ~p~n", [What])
      end;

    {tcp_closed, Socket} ->
      io:format("tcp::Socket ~p closed~n", [Socket]);

    {tcp_error, Socket, Reason} ->
      io:format("tcp::Socket ~p error reason: ~p~n", [Socket, Reason]),
      gen_tcp:close(Socket);

    {send, Payload} ->
      io:format("tcp::Socket ~p sending message: ~p~n", [Socket, Payload]),
      gen_tcp:send(Socket, Payload),
      listen_loop(Socket, HandlerFn, State);

    What ->
      io:format("tcp::unknown message ~p~n", [What])
  end.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------
