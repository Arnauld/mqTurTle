%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. mai 2016 09:20
%%%-------------------------------------------------------------------
-module(mqtterl_topic).
-author("Arnauld").

-record(compiled_terms, {tokens = []}).
-type compiled_terms() :: [term()].

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([match/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec(match(Topic :: binary(), Filter :: binary()) -> match | no_match | invalid_topic).
match(Topic, Filter) when is_binary(Filter), is_binary(Topic) ->
  CompiledTopic = compile_terms(Topic),
  CompiledFilter = compile_terms(Filter),
  match_terms(CompiledTopic, CompiledFilter).


compile_terms(Terms) when is_binary(Terms) ->
  [Terms].

match_terms([], []) ->
  true;
match_terms([H | TopicTail], [H | FilterTail]) ->
  match_terms(TopicTail, FilterTail);
match_terms(_, _) ->
  false.

