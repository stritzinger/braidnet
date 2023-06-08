# braidnet

An OTP application to spawn containers and orchestrate comunication between them and their peers hosted in other braidnet remote instances.

## Build

    rebar3 compile

## Run

    rebar3 shell

## Try

    braidnet:test_nodes().

## Deploy onto Fly.io
    flyctl deploy

## Erlang shell on Fly.io
SSH to machine, for example:

    flyctl machines list
    flyctl ssh console -a braidnet-ams -A fdaa:0:e75d:a7b:141:1445:ab0b:2

Start Erlang shell:

    remshell

## TODO:
- Tests!
- EPMD:
  - Check in names/1 if node is alive
  - Clean up state when node goes down
