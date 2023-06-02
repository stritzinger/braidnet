# braidnet

An OTP application to spawn containers and orchestrate comunication between them and their peers hosted in other braidnet remote instances.

## Build

    rebar3 compile

## Run

    rebar3 shell

## Try

    braidnet:test_nodes().

## TODO:
- Tests!
- EPMD:
  - Clean up names/1
  - Check in names/1 if node is alive
  - Clean up state when node goes down
  - Simpler, consistent data types (binaries, strings, atoms)
    - better workaround for json<->tuple limitation
  - Handle node names consistently (with or without the domain part?)
    - how to specify node names in the user config?
  - Simpler state record?
    - will (partially?) move to Mnesia anyway
