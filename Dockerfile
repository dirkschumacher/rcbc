FROM rocker/r-base
ENV TERM=xterm-256color

RUN apt-get update && apt-get -yq install vim \
    && apt-get -yq install coinor-libcbc-dev coinor-libclp-dev

RUN apt-get update && apt-get -yq install libssl-dev libcurl4-openssl-dev wget libpq-dev libxml2 r-cran-xml libxml2-dev

COPY install_packages.R /code/
RUN cd /code && Rscript ./install_packages.R

COPY tests/testthat/test-bad-index.R /code/
COPY tests/testthat/data-bad-index.rda /code/
