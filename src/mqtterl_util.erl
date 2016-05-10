%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. mai 2016 22:59
%%%-------------------------------------------------------------------
-module(mqtterl_util).
-author("Arnauld").


%% Variant, corresponds to variant 1 0 of RFC 4122.
-define(VARIANT10, 2#10).
-define(UUIDv4, 4).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([random_uuid/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

random_uuid() ->
  % Set the four most significant bits (bits 12 through 15) of the
  % time_hi_and_version field to 0100, corresponding to version 4.
  %
  % Set the two most significant bits (bits 6 and 7) of the
  % clock_seq_hi_and_reserved to zero and one, respectively.
  % Corresponding to variant 1 0.
  %
  % Set all other bits pseudo-randomly chosen values
  % (as generated by caller).
  <<U0:32, U1:16, _:4, U2:12, _:2, U3:30, U4:32>> = crypto:rand_bytes(16),

  <<U0:32, U1:16, ?UUIDv4:4, U2:12, ?VARIANT10:2, U3:30, U4:32>>.