# braidnet

An OTP application to spawn containers and orchestrate communication between them
and their peers hosted by remote Braidnet instances.

Deploy Braidnet onto host machines (currently Fly.io), build Erlang
applications that run [Braidnode](https://github.com/stritzinger/braidnode),
and use [Braidclient](https://github.com/stritzinger/braidclient) to define
your node connections and deploy your applications.

Use [Braidcert](https://github.com/stritzinger/braidcert) to
secure the Erlang distribution between Braidnet instances and Braidnode apps.

## Build

    rebar3 compile

## Run locally
Start [Braidcert](https://github.com/stritzinger/braidcert)
first in a separate shell, then:

    rebar3 shell

## Deploy
Braidnet will only be able to start up if it can connect to a
[Braidcert](https://github.com/stritzinger/braidcert) instance.

For now, Braidnet is meant to be deployed on [Fly.io](https://fly.io).
Create a new Fly application, replace the value of the `app` field
in the `fly.toml` file in this repo with your Fly app's name,
and deploy using `flyctl`.

## Use
To interface with Braidnet and deploy applications, use
[Braidclient](https://github.com/stritzinger/braidclient).


## Configuration
When Braidnet is ran via the rebar3 shell, `config/shell.config` applies.

When the relx release is ran, `config/container.config.src` (and
`config/container.vm.args.src`) apply.

The possible configuration values are:
```erlang
[
  {braidnet, [
    % HTTP Bearer authetication token between Braidclient and Braidnet:
    {rest_api_token, binary()},
    % Braidcert Fly.io app URL:
    {braidcert_url, string()},
    % HTTP Bearer authetication token between Braidcert and Braidnet:
    {braidcert_key, string()}
    % Enable only accepting signed Docker images:
    {docker_trust, boolean()}
  ]}
].
```
