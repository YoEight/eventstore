FROM ubuntu:16.04
MAINTAINER Yorick LAUPA <yo.eight@gmail.com>

RUN apt-get update -q && \
    apt-get install -qy curl git locales

RUN locale-gen en_US.UTF-8 && \
    echo 'LANG="en_US.UTF-8"' > /etc/default/locale

RUN curl -sSL https://get.haskellstack.org/ | sh

ENV PATH $PATH:/root/.local/bin
ENV LANG en_US.UTF-8

RUN stack setup

CMD ["ls"]
