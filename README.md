# braidnet

An OTP application to spawn containers and orchestrate comunication between them and their peers hosted in other braidnet remote instances.

## Build

    rebar3 compile

## Run
Start Braidcert first in a separate shell, then:

    rebar3 shell

## Try

    braidnet:test_nodes().

## Deploy onto Fly.io
    flyctl deploy

## Erlang shell on Fly.io
SSH to machine, for example:

    flyctl machines list
    flyctl ssh console -A fdaa:0:e75d:a7b:13b:5fb:6e00:2

Start Erlang shell:

    remshell
