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
