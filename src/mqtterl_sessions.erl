%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. mai 2016 23:01
%%%-------------------------------------------------------------------
-module(mqtterl_sessions).
-author("Arnauld").
-behaviour(gen_server).

-include("mqtterl_core.hrl").

%% API
-export([start_link/0, stop/0]).
-export([get_or_create/2, create/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sessions}).

%%%===================================================================
%%% API
%%%===================================================================
get_or_create(ClientId, Opts) ->
  gen_server:call(?SERVER, {get_or_create, ClientId, Opts}).

create(ClientId, Opts) ->
  gen_server:call(?SERVER, {create, ClientId, Opts}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> ok).
stop() ->
  gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{
    sessions = dict:new()
  }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%---------------------------------------------------------------------
handle_call({get_or_create, ClientId, Opts}, _From, State = #state{sessions = Sessions}) ->
  {NewSession, WasPresent, NewState} =
    case dict:find(ClientId, Sessions) of
      {ok, Session} ->
        {Session, true, State};

      error ->
        Session = new_session(Opts),
        NS = dict:store(ClientId, Session, Sessions),
        {Session, false, State#state{sessions = NS}}
    end,
  {reply, {NewSession, WasPresent}, NewState};

handle_call({create, ClientId, Opts}, _From, State = #state{sessions = Sessions}) ->
  WasPresent = case dict:find(ClientId, Sessions) of
                 {ok, _} ->
                   true;
                 error ->
                   false
               end,
  Session = new_session(Opts),
  NS = dict:store(ClientId, Session, Sessions),
  NewState = State#state{sessions = NS},
  {reply, {Session, WasPresent}, NewState};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

new_session(Opts) ->
  #session{id = mqtterl_util:random_uuid(), opts = Opts}.
