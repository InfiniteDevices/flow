FROM ubuntu:18.04

ENV DEBIAN_FRONTEND noninteractive
USER root
RUN apt-get update && \
    apt-get -y install gcc mono-mcs mingw-w64 && \
    rm -rf /var/lib/apt/lists/*

RUN  useradd -ms /bin/bash open62541

RUN mkdir -p /var/open62541-linux64 \
 && chown -R open62541:open62541 /var/open62541-linux64    

#ENV HOME /var/open62541-linux64

USER open62541
    
COPY open62541-linux64/ /var/open62541-linux64

WORKDIR /var/open62541-linux64

RUN gcc -std=c99 open62541.c myServer.c -o myServer
RUN gcc -std=c99 open62541.c myServer.c -lws2_32 -o myServer.exe
CMD ./myServer

