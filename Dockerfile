FROM erlang:21

RUN mkdir /var/embl

COPY _build/default/rel/genesearcher /var/embl/genesearcher

ENV SERVICE_NAME genesearcher

EXPOSE 8888

CMD /var/embl/genesearcher/bin/genesearcher foreground
