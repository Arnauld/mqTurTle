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

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([match/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec(match(Filter :: binary(), Topic :: binary()) -> match | no_match | invalid_topic).
match(Filter, Topic) when is_binary(Filter), is_binary(Topic) ->
  CompiledTopic = compile_terms(Topic),
  CompiledFilter = compile_terms(Filter),
  match_terms(CompiledFilter, CompiledTopic).


compile_terms(Terms) when is_binary(Terms) ->
  binary:split(Terms, <<"/">>, [global]).

match_terms([], []) ->
  match;
match_terms([H | FilterTail], [H | TopicTail]) ->
  match_terms(FilterTail, TopicTail);
match_terms([<<"#">>], _) -> % wildcard must be the last char of the topic to be valid
  match;
match_terms([<<"+">> | FilterTail], [_ | TopicTail]) ->
  match_terms(FilterTail, TopicTail);
match_terms(_, _) ->
  no_match.

