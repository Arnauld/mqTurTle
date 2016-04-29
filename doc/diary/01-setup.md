# Blabla

J'ai toujours été intrigué par les systèmes distribués, leurs complexités et leurs problématiques.
Si vous aussi ça vous intrigue...
Soit vous avez l'occasion de travaillé dans un contexte de ce type et vous devez résoudre certaines de ces problématiques,
soit vous travaillez pour une société
soit vous imaginez au travers de vos lectures leurs complexités.

# Setup

* install erlang
* install rebar3 `wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3` and add it in your PATH


    $ erl
    Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
    Eshell V7.2.1  (abort with ^G)
    
    Press <ctrl+c> to exit
    
    $ rebar3 -v
    rebar 3.1.0 on Erlang/OTP 18 Erts 7.2.1

* create a new project `rebar3 new app mqtterl`
* initial project layout can be found at commit `0ce7bee03cc8aeab3bdb21c831730cd7cfe73223`


## Dummy test

`test/dummy_tests.erl` 

**Note:** 
Test file should ends with `_tests` and test function should ends with `_test`

```erlang
-module(dummy_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_illustrate_a_basic_assert__test() ->
  Ref = ok,
  ?assertEqual(ok, Ref).
```

Verbose mode for tests has been activated to see what is really executed :smile: 
as well as code coverage to help to improve quality.

`rebar.config`

```
{eunit_opts, [verbose]}.

{cover_enabled, true}.
{cover_opts, [verbose]}.
{cover_print_enabled, true}.
{cover_export_enabled, true}.
```

Run tests

    $ rebar3 eunit
    ===> Verifying dependencies...
    ===> Compiling mqtterl
    ===> Performing EUnit tests...
    ======================== EUnit ========================
    file "mqtterl.app"
      application 'mqtterl'
        module 'mqtterl_app'
        module 'mqtterl_sup'
        [done in 0.012 s]
      [done in 0.022 s]
    dummy_tests: should_illustrate_a_basic_assert__test (module 'dummy_tests')...ok
    =======================================================
      Test passed.
    
Have a look at the coverage

    $ rebar3 cover
    ===> Performing cover analysis...
      |------------------------|------------|
      |                module  |  coverage  |
      |------------------------|------------|
      |           mqtterl_app  |        0%  |
      |           mqtterl_sup  |        0%  |
      |           dummy_tests  |      100%  |
      |------------------------|------------|
      |                 total  |       33%  |
      |------------------------|------------|
      coverage calculated from:
        /Users/Arnauld/Projects/mqtterl/_build/test/cover/eunit.coverdata
      cover summary written to: /Users/Arnauld/Projects/mqtterl/_build/test/cover/index.html
      
      
## a basic TCP server

commit `bbd31ed1d88401af5d203dd569dc6218c4b16a3a`

## parse/encode utf8

**MQTT 1.5.3 UTF-8 encoded strings**

commit `297b83ed71259bb46d3db47af29494db3e7a513e`

## record frame

Idea is to start a tcp server that will be used to dump calls from `client_test` 
from https://github.com/eclipse/paho.mqtt.testing

see https://wiki.eclipse.org/Interop_Testing_Plan#Test_Broker

commit `9941e68a5c837ff902ae8f0b582a28be4bed152d`


`Terminal 1`: starts the "broker" on port 10305

    $ erl
    Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
    
    Eshell V7.2.1  (abort with ^G)
    1> c('src/mqtterl_tcp_dump.erl').
    src/mqtterl_tcp_dump.erl:24: Warning: variable 'Ref' is unused
    {ok,mqtterl_tcp_dump}
    2> c('src/mqtterl_tcp_server.erl').
    {ok,mqtterl_tcp_server}
    3> mqtterl_tcp_dump:start(10305).
    Ready to accept connection on port 10305
    {ok,{state,#Port<0.2398>,
               {#Fun<mqtterl_tcp_dump.0.104469679>,
                #Fun<mqtterl_tcp_dump.1.104469679>}}}
    4>

Clone `paho.mqtt.testing` project

    $ git clone https://github.com/eclipse/paho.mqtt.testing.git
   

`Terminal 2`: starts MQTT proxy

    $ cd paho.mqtt.testing/interoperability
    $ python3 run_proxy.py localhost 10305

`Terminal 3`: starts MQTT client test


    $ cd paho.mqtt.testing/interoperability
    $ python3 client_test.py -p 10305
    
And of course client is crashing since our broker is not responding anything...
Let's see the output of our broker to grab the frame and the mqtt proxy to grab the mqtt data sent.

`Terminal 1`

    Connection accepted #Port<0.2399>
    handlerFn:Init
    Got packet: <<16,22,0,4,77,81,84,84,4,2,0,0,0,10,109,121,99,108,105,101,110,
                  116,105,100>>
    4>
    =ERROR REPORT==== 29-Apr-2016::09:19:30 ===
    Error in process <0.47.0> with exit value:
    {{badmatch,<<16,22,0,4,77,81,84,84,4,2,0,0,0,10,109,121,99,108,105,101,110,
                 116,105,100>>},
     [{mqtterl_tcp_dump,'-start/1-fun-1-',3,
                        [{file,"src/mqtterl_tcp_dump.erl"},{line,30}]},
      {mqtterl_tcp_server,listen_loop,3,
                          [{file,"src/mqtterl_tcp_server.erl"},{line,76}]}]}
    ** exception error: no match of right hand side value <<16,22,0,4,77,81,84,84,4,2,0,0,0,10,109,121,99,108,105,
                                                            101,110,116,105,100>>
         in function  mqtterl_tcp_dump:'-start/1-fun-1-'/3 (src/mqtterl_tcp_dump.erl, line 30)
         in call from mqtterl_tcp_server:listen_loop/3 (src/mqtterl_tcp_server.erl, line 76)

So frame received `<<16,22,0,4,77,81,84,84,4,2,0,0,0,10,109,121,99,108,105,101,110,116,105,100>>` 
which means according to `Terminal 2`:

    None, None, None) True
    Traceback (most recent call last):
      File "/Users/Arnauld/Projects/3rdParties/mqtt/paho.mqtt.testing/interoperability/mqtt/proxy/mqttsas.py", line 46, in handle
        brokers.connect((brokerhost, brokerport))
    ConnectionRefusedError: [Errno 61] Connection refused
    20160429 092405.816887 C to S myclientid Connects(DUP=False, QoS=0, Retain=False, ProtocolName=MQTT, ProtocolVersion=4, CleanSession=True, WillFlag=False, KeepAliveTimer=0, ClientId=myclientid, usernameFlag=False, passwordFlag=False)
    ['0x10', '0x16', '0x0', '0x4', '0x4d', '0x51', '0x54', '0x54', '0x4', '0x2', '0x0', '0x0', '0x0', '0xa', '0x6d', '0x79', '0x63', '0x6c', '0x69', '0x65', '0x6e', '0x74', '0x69', '0x64']
    b'\x10\x16\x00\x04MQTT\x04\x02\x00\x00\x00\nmyclientid'
    20160429 092405.81721 client myclientid connection closing
    
The interesting part is:
`Connects(DUP=False, QoS=0, Retain=False, ProtocolName=MQTT, ProtocolVersion=4, CleanSession=True, WillFlag=False, KeepAliveTimer=0, ClientId=myclientid, usernameFlag=False, passwordFlag=False)`

Note for later: framing looks differents betwwen `Terminal 2` and `Terminal 1`, let's ignore that for the moment, this may come from: endianess, signedness or packing (8bit vs 16bit)...
  
We actually reached to grab a real frame with a known contents, so let's back to our packet parser.
And try to parse the **Fixed Header** (**MQTT 2.2 Fixed Header**)

`test/mqtterl_parser_tests.erl`

```erlang
should_parse_connect_packet__test() ->
  %% `Connects(DUP=False, QoS=0, Retain=False, 
  %%           ProtocolName=MQTT, ProtocolVersion=4, 
  %%           CleanSession=True, WillFlag=False, KeepAliveTimer=0, 
  %%           ClientId=myclientid, usernameFlag=False, passwordFlag=False)`
  Packet = <<16, 22, 0, 4, 77, 81, 84, 84, 4, 2, 0, 0, 0, 10, 109, 121, 99, 108, 105, 101, 110, 116, 105, 100>>,
  {Header, Remaining} = mqtterl_parser:parse_header(Packet),
  ?assertEqual(?CONNECT, Header#mqtt_fixed_header.type).
```

So will add header definitions into an include file, so that it will be easier to share among the project.
Those are similar to h file in c.


Issue: one cannot read a byte partially and let the remaining in a binary thus:

```erlang
parse_type(Bin) ->
  <<PacketType:4, Rest/binary>> = Bin,
  {PacketType, Rest}.
```

results in:

```
in call from mqtterl_parser_tests:should_parse_connect_packet__test/0 (/Users/Arnauld/Projects/mqtterl/_build/test/lib/mqtterl/test/mqtterl_parser_tests.erl, line 35)
**error:{badmatch,<<16,22,0,4,77,81,84,84,4,2,0,0,0,10,109,121,99,...>>}
  output:<<"">>
```

So let's grab the entire byte and load the flags too.


