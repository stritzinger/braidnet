
-type container_id() :: binary().

-record(container, {
    image               :: binary(),
    status = unknown    :: unknown | running | lost
}).