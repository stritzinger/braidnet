#!/bin/ash

exec /opt/braidnet/erts-14.0/bin/erl \
    -boot /opt/braidnet/releases/0.1.0/start_clean \
    -name remote@"$(hostname)".vm."$FLY_APP_NAME".internal \
    -remsh braidnet@"$(hostname)".vm."$FLY_APP_NAME".internal \
    -setcookie cookie \
    -hidden \
    -dist_listen false
