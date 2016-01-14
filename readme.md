# TCP Chat

Simple chat (OTP application using Ranch)

### compile
```bash
rebar get-deps
rebar compile
```
### run
```bash
erl -sname chat -pa ./deps/ranch/ebin -pa ./ebin -eval "application:ensure_all_started(chat)"
```
### use
|command|use|
|---|---|
|\help|shows help (a list of commands)|
|\greetme|says "hi" |
|\name NAME|adds "NAME:" to every message sent via this connection|