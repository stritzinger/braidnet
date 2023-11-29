# braidnet

An OTP application to spawn containers and orchestrate communication between them
and their peers hosted by remote braidnet instances.

## Main steps
We currently only support [Fly.io](https://fly.io) as cloud infrastracture.

1. Deploy a single [braidcert](https://github.com/stritzinger/braidcert) instance to be the root of trust in the internal PKI.
2. Deploy `braidnet` onto host machines.
3. Build docker conteiners with your Erlang Apps using [braidnode](https://github.com/stritzinger/braidnode).
4. Finally, use [braid](https://github.com/stritzinger/braid) to define
your node connections and deploy your applications.

## Build

    rebar3 compile

## Run locally
Start [braidcert](https://github.com/stritzinger/braidcert)
first in a separate shell, then:

    rebar3 shell

## Deploy
Braidnet will only be able to start up if it can connect to a
[braidcert](https://github.com/stritzinger/braidcert) instance.

For now, braidnet is meant to be deployed on [Fly.io](https://fly.io).
Create a new Fly application, replace the value of the `app` field
in the `fly.toml` file in this repo with your Fly app's name,
and deploy using `flyctl`.

## Use
To interface with braidnet and deploy applications, use
[braid](https://github.com/stritzinger/braid).


## Configuration
When braidnet is ran via the rebar3 shell, `config/shell.config` applies.

When the relx release is ran, `config/container.config.src` (and
`config/container.vm.args.src`) apply.

The possible configuration values are:
```erlang
[
  {braidnet, [
    % HTTP Bearer authetication token between braid and braidnet:
    {rest_api_token, binary()},
    % braidcert Fly.io app URL:
    {braidcert_url, string()},
    % HTTP Bearer authetication token between braidcert and braidnet:
    {braidcert_key, string()}
    % Enable only accepting signed Docker images:
    {docker_trust, boolean()}
  ]}
].
```
