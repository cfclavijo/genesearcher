PROJECT			  = geneserver
REBAR	        = rebar3
REBAR_PROFILE = default

.PHONY: all deps compile test eunit

all: compile

deps:
	$(REBAR) get-deps

me:
	$(REBAR) compile

compile:
	$(REBAR) compile

clean:
	find _build -name "*.beam" -exec rm {} \;

fresh:
	rm -rf _build

run: clean
	$(REBAR) shell

eunit:
	export ERL_FLAGS=-config $(PWD)/eunit

test: eunit
	$(REBAR) covertool generate
	cp _build/test/covertool/$(PROJECT).covertool.xml $(PROJECT).coverage.xml

docker: fresh
	docker run -it -v $(PWD):/tmp -w /tmp erlang:21 /tmp/build.sh

docker-run:
	docker run -it --rm -p 8080:8080 genesearcher
