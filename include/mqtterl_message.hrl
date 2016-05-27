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
  % HEADER
  protocol_name :: binary(),
  protocol_level :: non_neg_integer(),
  has_username :: boolean(),
  has_password :: boolean(),
  will_retain :: boolean(),
  will_qos :: integer(),
  will_flag :: boolean(),
  clean_session :: 0|1,
  keep_alive :: 0|1,
  reserved_flag :: 0|1,
  % PAYLOAD
  client_id = undefined :: binary() | undefined,
  username = undefined :: binary() | undefined,
  password = undefined :: binary() | undefined,
  will_topic = undefined :: binary() | undefined,
  will_message = undefined :: binary() | undefined}).

-define(CONNACK_ACCEPT, 0).    %% Connection accepted
-define(CONNACK_PROTO_VER, 1).    %% Connection Refused, unacceptable protocol version: The Server does not support the level of the MQTT protocol requested by the Client
-define(CONNACK_INVALID_ID, 2).    %% Connection Refused, identifier rejected: Client Identifier is correct UTF-8 but not allowed by the Server
-define(CONNACK_SERVER, 3).    %% Connection Refused, Server unavailable: The Network Connection has been made but the MQTT service is unavailable
-define(CONNACK_CREDENTIALS, 4).    %% Connection Refused, bad user name or password: The data in the user name or password is malformed
-define(CONNACK_AUTH, 5).    %% Connection Refused, not authorized: Client is not authorized to connect

-type mqtt_connack() :: ?CONNACK_ACCEPT..?CONNACK_AUTH.

-record(mqtt_connack, {
  session_present = false :: boolean(),
  return_code = ?CONNACK_SERVER :: mqtt_connack()
}).

-record(mqtt_disconnect, {}).

-record(mqtt_subscribe, {
  packet_id :: integer(),
  topic_filters = [] :: [binary()]
}).


-define(QOS0, 0). %% At most once
-define(QOS1, 1). %% At least once
-define(QOS2, 2). %% Exactly once

-type mqtt_qos() :: ?QOS0..?QOS2.

-define(SUBACK_FAILURE, 16#80). %% Number base#value

-record(mqtt_suback, {
  packet_id :: integer(),
  return_codes :: [mqtt_qos() | ?SUBACK_FAILURE]
}).

-record(mqtt_publish, {
  dup = false :: boolean(),
  qos = ?QOS0 :: mqtt_qos(),
  retain = false :: boolean(),
  packet_id :: integer(),
  topic :: binary(),
  payload :: binary()
}).

-record(mqtt_puback, {
  packet_id :: integer()
}).


-record(mqtt_pubrec, {
  packet_id :: integer()
}).

-record(mqtt_pubrel, {
  packet_id :: integer()
}).

-record(mqtt_pubcomp, {
  packet_id :: integer()
}).







