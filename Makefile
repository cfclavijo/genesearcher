PROJECT			  = genesearcher
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

eunit-config:
	ERL_FLAGS='-config eunit.config' $(REBAR) eunit

test: eunit-config
	$(REBAR) cover
	$(REBAR) covertool generate
	cp _build/test/covertool/$(PROJECT).covertool.xml $(PROJECT).coverage.xml

docker-build: fresh
	docker run -it -v $(PWD):/tmp -w /tmp erlang:21 /tmp/build.sh

docker-image: docker-build
	docker build -t $(PROJECT):0.2.0 .

docker-run:
	docker run -it --rm -p 8080:8080 genesearcher:0.2.0
