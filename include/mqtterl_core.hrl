%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. mai 2016 19:27
%%%-------------------------------------------------------------------
-author("Arnauld").

-type topic_filter() :: binary().

-record(session, {
  id :: binary(),
  opts :: map(),
  subscriptions = [] :: [topic_filter()]}).