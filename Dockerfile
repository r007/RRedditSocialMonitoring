# Base image https://hub.docker.com/u/rocker/
FROM rocker/r-base:latest

## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libssh2-1-dev \
    libcurl4-openssl-dev \
    libssl-dev

## copy files
COPY install_packages.R /install_packages.R

## install R-packages
RUN Rscript /install_packages.R

## copy the script itself
COPY scraper.R /scraper.R
CMD ["Rscript", "/scraper.R"]