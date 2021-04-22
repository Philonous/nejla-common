# Dockerfile for running tests in

FROM ubuntu:focal

LABEL owner="Nejla AB"

# We need docker to run this image
RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y --no-install-recommends  \
      libicu-dev \
      libpq-dev \
      locales \
        && \
    rm -rf /var/lib/apt/lists/* && \
    localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

ENV LANG=en_US.UTF-8
