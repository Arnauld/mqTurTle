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

-export([start_link/2]).

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
      spawn_link(fun() ->
        io:format("Ready to accept connection on port ~p~n", [Port]),
        accept(State)
      end),
      {ok, State};

    {error, Reason} ->
      {stop, Reason}
  end.

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
  listen_loop(Socket, HandlerFn, HandlerInitFn()).

listen_loop(Socket, HandlerFn, State) ->
  %% set soscket options to receive messages directly into itself
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, NewData} ->
      io:format("Got packet: ~p~n", [NewData]),
      NewState = HandlerFn(Socket, State, NewData),
      listen_loop(Socket, HandlerFn, NewState);

    {tcp_closed, Socket} ->
      io:format("Socket ~p closed~n", [Socket]);

    {tcp_error, Socket, Reason} ->
      io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
  end.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------
