# syntax = docker/dockerfile:experimental

#--- Builder -------------------------------------------------------------------

ARG profile=container
ARG erlang_version=25.3.2.2
FROM erlang:$erlang_version-alpine as builder

WORKDIR /app/src
ENV REBAR_BASE_DIR /app/_build

# Install git for fetching non-hex depenencies.
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update \
        openssl \
        git \
        # Needed for NIFs:
        make \
        gcc \
        g++ \
        libc-dev \
        libbsd-dev

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock ./

# RUN apk add --no-cache openssh-client git && \
#     mkdir -p -m 0600 ~/.ssh && \
#     ssh-keyscan github.com >> ~/.ssh/known_hosts && \
#     git config --global url."git@github.com:".insteadOf "https://github.com/"

RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 compile

#--- Profile Builder -----------------------------------------------------------

FROM builder as builder-profile

ARG profile
RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as $profile compile

#--- Releaser ------------------------------------------------------------------

FROM builder-profile as releaser

# create the directory to unpack the release to
RUN mkdir -p /opt/rel

# tar for unpacking the target system
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    apk add --update tar

ARG profile
RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as $profile tar && \
    tar -zxvf $REBAR_BASE_DIR/$profile/rel/*/*.tar.gz -C /opt/rel

#--- Runner --------------------------------------------------------------------

FROM docker:24.0-dind as runner

WORKDIR /opt/braidnet/

    # write files generated during startup to /tmp
ENV RELX_OUT_FILE_PATH=/tmp \
    # braidnet specific env variables to act as defaults
    LOGGER_LEVEL=debug \
    SCHEDULERS=1

# openssl needed by the crypto app
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update \
        libstdc++ \
        zip \
        openssl \
        ncurses \
        curl

COPY --from=releaser /opt/rel .

# Alias remote shell script under a single command
RUN touch ~/.profile && \
    printf "%s\n" 'alias remshell='\''/opt/braidnet/erts-*/bin/erl -boot $(find /opt/braidnet/releases -type f -name start_clean.boot | sed "s/\.boot$//") -setcookie cookie -proto_dist inet6_tls -ssl_dist_optfile lib/braidnet-*/priv/prod.ssl_dist_opts.rel -sname console -remsh braidnet@$(hostname)'\''' \
    >> ~/.profile

EXPOSE 80/tcp

ENTRYPOINT ["/opt/braidnet/bin/braidnet"]
CMD ["foreground"]
