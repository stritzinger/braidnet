# Braidnet - Braid REST API

All methods calls require an authentication token.

    "Authorizaiton: Bearer Token"

For fly.io, the request must provide the fly.io machine in a special header

    "fly-force-instance-id: the-fly-machine-id"

Most methods require a configuration that describes the topology

This is an example of a valid json configuration

    {
        "e28650eeb01e68" :
            {
                "alice" => #{
                    image => "namespace/braidnode",
                    "connections" => ["bob@5683929b651208"]
                }
            },
        "5683929b651208" :
            {
                "bob" : {
                    "image" : <<"namespace/braidnode">>,
                    "connections" : ["alice@e28650eeb01e68"]
                }
            }
    }

## Methods

<details>
 <summary><code>GET</code> <code><b>/api/instances</b></code> - lists all braidnet instances that compose the cluster  </summary>

##### Parameters

Note: this methos does not need `"fly-force-instance-id"` since can solved by any instance

##### Responses

> | http code     | content-type                      | response                                              |
> |---------------|-----------------------------------|------------------------------------------------------|
> | `200`         | `application/json`   | `json list`|

    ["5683929b651208", "e28650eeb01e68", ...]

</details>

<details>
 <summary><code>GET</code> <code><b>/api/list</b></code> - list all containers managed from the server instance </summary>

##### Parameters

none

##### Responses

> | http code     | content-type                      | response                                              |
> |---------------|-----------------------------------|------------------------------------------------------|
> | `200`         | `application/json`   | `json list`|
    [
        { "5683929b651208" :
            [
                {
                    "id": "b61241b0t5...",
                    "image": "local/braidnode",
                    "name": "bobby",
                    "status": "unknown"
                },
                {
                    "id": "n34hgf934gn...",
                    "image": "local/bigmac",
                    "name": "chad",
                    "status": "running"
                }
            ]
        },
        { "e28650eeb01e68" :
            [...]
        }
    ]
</details>

<details>
 <summary><code>GET</code> <code><b>/api/logs</b></code> - get the log dump of a single container</summary>

##### Parameters

> | name      |  type     | data type               | description                                                           |
> |-----------|-----------|-------------------------|-----------------------------------------------------------------------|
> | `cid`  |  required | query string   |  The ID of the container |

##### Responses

> | http code     | content-type                      | response                                              |
> |---------------|-----------------------------------|------------------------------------------------------|
> | `200`         | `application/json`   | `a simple string`|
> | `400`         | `...`   | `...`|
> | `404`         | `...`   | `...`|


</details>
<details>
 <summary><code>GET</code> <code><b>/api/rpc</b></code> - execute a synchronous remote procedure call on the erlang node executing in a container </summary>

##### Parameters

> | name      |  type     | data type       | description          |
> |-----------|-----------|-------------------------|-----------|
> | `cid`  |  required | query string   |  The ID of the container |
> | `m`  |  required | query string   |   Base64 Encoded module atom (term in binary)  |
> | `f`  |  required | query string   |   Base64 Encoded function atom (term in binary) |
> | `args`  |  required | query string   |  Base64 Encoded list of arguments (term in binary) |


##### Responses

> | http code     | content-type                      | response                                              |
> |---------------|-----------------------------------|------------------------------------------------------|
> | `200`         | `application/json`   | `result`|

</details>

<details>
 <summary><code>POST</code> <code><b>/api/launch</b></code> - issue deployment of a new braid configuration</summary>

##### Parameters

Braid configuration for braidnet in json format.

##### Responses

> | http code     |   Status
> |---------------|-------------
> | `204`         | `No Content`

</details>

<details>
 <summary><code>DELETE</code> <code><b>/api/destroy</b></code> - removes all containers given a braidnet configuration</summary>

This issues an ordered shutdown while deleting all internal data about the container.

##### Parameters

Braid configuration for braidnet in json format.

##### Responses

> | http code     |   Status
> |---------------|-------------
> | `204`         | `No Content`

</details>


