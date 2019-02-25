genesearcher
====

REST Application to allow users to search for a gene by name.

Build
----

    $ rebar3 compile
    
Execute
----
The service will start under port 8080.

    $ rebar3 shell
    
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
----
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
    
