FROM jgoerzen/debian-base-minimal:buster

COPY ./dist/package/debian-10 /package/dist

RUN apt-get update -qq
RUN apt-get -y -f install "/package/dist/$(ls /package/dist)"
RUN rm -r /package/dist

ENTRYPOINT [ "ligo" ]