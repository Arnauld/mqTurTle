%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. mai 2016 00:16
%%%-------------------------------------------------------------------
-ifndef(ASSERT_EXT_HRL).
-define(ASSERT_EXT_HRL, true).

%% This is a convenience macro which gives more detailed reports when
%% the expected LHS value is not a pattern, but a computed value
-ifdef(NOASSERT).
-define(assertEqualWithMessage(Message, Expect, Expr), ok).
-else.
-define(assertEqualWithMessage(Message, Expect, Expr),
  begin
    ((fun(__X) ->
      case (Expr) of
        __X -> ok;
        __V -> erlang:error({assertEqualWithMessage,
          [{module, ?MODULE},
            {line, ?LINE},
            {expression, (??Expr)},
            {expected, __X},
            {value, __V},
            {message, (Message)}]})
      end
    end)(Expect))
  end).
-endif.

-endif.  % ASSERT_EXT_HRL