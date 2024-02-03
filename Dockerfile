FROM rocker/r-ver:4.3.2

ENV RENV_PATHS_LIBRARY renv/library

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libgit2-dev \
    libudunits2-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev

# Install renv and restore packages from renv.lock
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# COPY renv.lock /usr/local/src/fiscalmonitor/
# WORKDIR /usr/local/src/fiscalmonitor
# RUN R -e "renv::restore()"

COPY . /usr/local/src/fiscalmonitor
WORKDIR /usr/local/src/fiscalmonitor
RUN R -e "renv::restore()"

# Expose dynamic port
EXPOSE $PORT

CMD R -e "options(shiny.host='0.0.0.0');source('app.R');fiscalmonitor::run_app()"
