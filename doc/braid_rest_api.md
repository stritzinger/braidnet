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
                    "epmd_port" => "43591",
                    "connections" => ["bob"]
                }
            },
        "5683929b651208" :
            {
                "bob" : {
                    "image" : <<"namespace/braidnode">>,
                    "epmd_port" : "43591",
                    "connections" : ["alice"]
                }
            }
    }

## Methods

<details>
 <summary><code>GET</code> <code><b>/api/list</b></code> - list all containers managed from all instances in the config</summary>

##### Parameters

Braid configuration for braidnet in json format.

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
> | `container-id`  |  required | query string   |  The ID of the container |

##### Responses

> | http code     | content-type                      | response                                              |
> |---------------|-----------------------------------|------------------------------------------------------|
> | `200`         | `application/json`   | `a simple string`|
> | `400`         | `...`   | `...`|
> | `404`         | `...`   | `...`|


</details>

<details>
 <summary><code>POST</code> <code><b>/api/launch</b></code> - deploy a new braid configuration</summary>

##### Parameters

Braid configuration for braidnet in json format.

##### Responses

> | http code     | content-type  |   response                                              |
> |---------------|--------------|----------
> | `200`         | `application/json`   | `"ok"`

</details>

<details>
 <summary><code>DELETE</code> <code><b>/api/destroy</b></code> - removes all containers given a braidnet configurationr</summary>

This issues an ordered shutdown while deleting all internal data about the container.

##### Responses

> | http code     | content-type  |   response                                              |
> |---------------|--------------|----------
> | `200`         | `application/json`   | `"ok"`

</details>

## WIP

<details>
 <summary><code>POST</code> <code><b>/api/pause</b></code> - stops a list of containers</summary>

##### Parameters

    [container_a, container_b]


##### Responses

> | http code     | content-type  |   response                                              |
> |---------------|--------------|----------
> | `200`         | `application/json`   | `"ok"`

</details>

<details>
 <summary><code>POST</code> <code><b>/api/unpause</b></code> - resumes execution of a list of containers</summary>

##### Parameters

    [container_a, container_b]


##### Responses

> | http code     | content-type  |   response                                              |
> |---------------|--------------|----------
> | `200`         | `application/json`   | `"ok"`

</details>
