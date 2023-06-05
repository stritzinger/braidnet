# Braidnet - Braid REST API


All methods require a configuration that describes the topology

Braid configuration for braidnet in json format:

    #{
        'e28650eeb01e68' =>
            #{
                alice => #{
                    image => <<"namespace/braidnode">>,
                    epmd_port => <<"43591">>,
                    connections => [bob]
                }
            },
        '5683929b651208' =>
            #{
                bob => #{
                    image => <<"namespace/braidnode">>,
                    epmd_port => <<"43591">>,
                    connections => [alice]
                }
            }
    }.

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
 <summary><code>POST</code> <code><b>/api/launch</b></code> - deploy a new braid configuration</summary>

##### Parameters

Braid configuration for braidnet in json format.

##### Responses

> | http code     | content-type  |   response                                              |
> |---------------|--------------|----------
> | `200`         | `application/json`   | `"ok"`

</details>



<details>
 <summary><code>DELETE</code> <code><b>/api/destroy</b></code> - removes all containers given a braidnet configuration </summary>

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