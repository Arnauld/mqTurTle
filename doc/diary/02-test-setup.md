Terminal 1:

    $ rebar3 shell
    ===> Verifying dependencies...
    ===> Compiling mqtterl
    Erlang/OTP 18 [erts-7.2.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V7.2.1  (abort with ^G)
    1> mqtterl_sessions:start_link(), mqtterl_tcp_server:start_link(10305, mqtterl_protocol:tcp_handler()).
    Ready to accept connection on port 10305
    {ok,{state,#Port<0.39501>,
               {#Fun<mqtterl_protocol.tcp_handler_init.0>,
                #Fun<mqtterl_protocol.tcp_on_packet.3>}}}

Terminal 2:

    $ cd Projects/3rdParties/mqtt/paho.mqtt.testing/interoperability
    $ python3 run_proxy.py localhost 10305
    
Terminal 3:

    $ cd Projects/3rdParties/mqtt/paho.mqtt.testing/interoperability
    $ python3 client_test.py -p 10306
    