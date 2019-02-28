genesearcher
====

REST Application to allow users to get *gene name suggestions* for a given *partial gene name*.

Dependencies
----
* Rebar3 & Erlang 21
The Service main language is erlang, you must install this if you want to develop and/or execute the service directly in your machine.

* **Docker**
This Service has been designed to be Dockerized. You can compile and execute it automatically inside a Docker container. *see Running Docker Image*

Getting the code
===
Just clone it or download it directly from github

    $ git clone

Docker-ish
====
Building the Docker Image
----
This process will download a base image (erlang 21), upload the code, build it inside and install the Service. This gives us the advantage of deploying the code through different OS.

A Docker image called **genesearcher** will be created

    $ make docker

Running Docker Image
----
With the service Dockerized it becomes easy to launch it in a proper environment (server or locally) by simply running

    $ make docker-run


Development
====

Build
----

    $ make
    
Execute
----
The service will start under port 8080.

    $ make run
    
Configuration
----
Update `erl.config` according to the required parameters.

    {genesearcher, [
        {'http_port', 8080},
        {'ensembldb_pool_size', 5},
        {'ensembldb_pool_max_overflow', 10},
        {'ensembldb_host', "ensembldb.ensembl.org"},
        {'ensembldb_port', 3306},
        {'ensembldb_database', "ensembl_website_90"},
        {'ensembldb_user', <<"anonymous">>},
        {'ensembldb_password', undefined},
        {'gene_suggest_limit', <<"10">>}
    ]}

Usage
====
-The service does not require authentication.-

#### gene_suggest
Given a partial name of a GENE and target species, returns a list of suggested gene names.

##### Example Usage
    $ curl http://localhost:8080/gensearcher/v1/gene_suggest/ -d '{"query":"brc", "species": ["homo_sapiens"], "limit": "13"}'
    
##### JSON Request Body Parameters

* `query`:String. The Partial query typed by the user.
* `species`:List(String). The name of the target species.
* `limit`:Integer. *Optional* The maximum number of suggestions to return.

##### Content Type
    application/json
    
##### 200 Ok
    {
        "suggestions": [
            "BRCA1",
            "BRCA2",
            "BRCC3",
            "BRCC3P1"
        ]
    }

##### 400 Invalid Parameters
