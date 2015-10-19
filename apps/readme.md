# TCP Chat

OTP application with ranch

### compile
```bash
rebar get-deps
rebar compile
```
### run
```bash
erl -pa deps/ranch/ebin apps/ebin
```
in then in a shell:
```erlang
application:start(ranch).
application:start(chat).
```
### use
|command|use|
|---|---|
|\help|shows help (a list of commands)|
|\greetme|says "hi" |
|\name NAME|adds "NAME:" to every message sent via this connection|