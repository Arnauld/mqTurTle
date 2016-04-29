%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. avr. 2016 23:02
%%%-------------------------------------------------------------------

-define(RESERVED, 0).   %% Reserved
-define(CONNECT, 1).   %% Client request to connect to Server
-define(CONNACK, 2).   %% Connect acknowledgment
-define(PUBLISH, 3).   %% Publish message
-define(PUBACK, 4).   %% Publish acknowledgment
-define(PUBREC, 5).   %% Publish received (assured delivery part 1)
-define(PUBREL, 6).   %% Publish release (assured delivery part 2)
-define(PUBCOMP, 7).   %% Publish complete (assured delivery part 3)
-define(SUBSCRIBE, 8).   %% Client subscribe request
-define(SUBACK, 9).   %% Server Subscribe acknowledgment
-define(UNSUBSCRIBE, 10).   %% Unsubscribe request
-define(UNSUBACK, 11).   %% Unsubscribe acknowledgment
-define(PINGREQ, 12).   %% PING request
-define(PINGRESP, 13).   %% PING response
-define(DISCONNECT, 14).   %% Client is disconnecting
-define(RESERVED2, 15).   %% Reserved

-type mqtt_packet_type() :: ?RESERVED..?DISCONNECT.

-record(mqtt_connect, {
  protocol_name :: string(),
  protocol_level :: pos_integer(),
  username :: boolean(),
  password :: boolean(),
  will_retain :: boolean(),
  will_qos :: integer(),
  will_flag :: boolean(),
  clean_session :: boolean(),
  keep_alive :: pos_integer()}).