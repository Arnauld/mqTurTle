mqtterl
=====

An OTP application

Build
-----

    $ # clean
    $ rebar3 clean
    
    $ # compile
    $ rebar3 compile
    
    $ # test (see note)
    $ rebar3 eunit
    
    $ # dialyzer / code analyzer
    $ rebar3 dializer
    
    $ # code coverage (see note)
    $ rebar3 cover
    
    $ # generate doc
    $ rebar3 doc
    

Note `rebar.config`

```
{cover_enabled, true}.
{cover_opts, [verbose]}.
{cover_print_enabled, true}.
{cover_export_enabled, true}.

{eunit_opts, [verbose]}.
```

Resources
=========

* http://www.hivemq.com/blog/mqtt-essentials-part-3-client-broker-connection-establishment
* http://vasters.com/clemensv/2014/06/02/MQTT+An+Implementers+Perspective.aspx
* https://www.genuitec.com/introduction-to-mqtt-protocol/
* https://wiki.eclipse.org/Interop_Testing_Plan#Test_Broker
* http://emauton.org/disterl/
