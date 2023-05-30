# Braidnet - Braid REST API

## Methods

<details>
 <summary><code>GET</code> <code><b>/api/list</b></code> - list all containers managed on the current braidnet instance</summary>

##### Parameters


> | name      |  type     | data type               |
> |-----------|-----------|-------------------------|


##### Responses

> | http code     | content-type                      | response                                              |
> |---------------|-----------------------------------|------------------------------------------------------|
> | `200`         | `application/json`   | `json list`|
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
</details>

<details>
 <summary><code>POST</code> <code><b>/api/launch</b></code> - deploy a new braid configuration</summary>

##### Parameters

Braid configuration for braidnet in json format

    {
        "orchestrator@braidnet1.fly.dev" :
            {
                "Bob" : {"image" : "local/braidnode", "connections" : []},
                "Alice" : {"image" : "local/fancynode", "connections" : []},
            },
        "orchestrator@braidnet2.fly.dev" :
            {
                "Milva" : {"image" : "local/oil", "connections" : []},
                "Frank" : {"image" : "local/beer", "connections" : []},
            }
    }

##### Responses

> | http code     | content-type  |   response                                              |
> |---------------|--------------|----------
> | `200`         | `application/json`   | `"ok"`

</details>

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

<details>
 <summary><code>DELETE</code> <code><b>/api/destroy</b></code> - removes all containers given a braidnet configuration </summary>

##### Parameter

The launch configuration used in `/api/launch`.
This does the opposite with a forcefull cleanup.
Each orcherstrator will kill all containers managed by itself.
This will run `docker kill`.

##### Responses

> | http code     | content-type  |   response                                              |
> |---------------|--------------|----------
> | `200`         | `application/json`   | `"ok"`

</details>

