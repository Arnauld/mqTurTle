mqtterl
=====

An OTP application

Build
-----

    $ rebar3 compile
    
# Developer's notes

## Clean and launch tests

```
$ rebar3 clean compile eunit
```

## Generate doc

```
$ rebar3 doc
```

## Execute dializer

```
$ make clean compile dialyzer
```

It may take some times the first time. 


### If dializer fails

```
→ make dialyzer
  Checking whether the PLT /Users/Arnauld/.dialyzer_plt is up-to-date...
dialyzer: Not a regular file: /usr/local/lib/erlang/lib/erts-6.0/ebin/erl_prim_loader.beam

make: *** [dialyzer] Error 1
Finished make dialyzer in 0 seconds
```

This may happens, if the `~/.dializer.plt` references old versions. Delete this file and retry.    
