FROM rocker/r-ver:4.1.0

RUN R -e "install.packages('remotes'); \
  remotes::install_version('sf', '0.9.8')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('jsonlite', '1.7.2')"

RUN R -e "install.packages('geojsonio'); \
  remotes::install_version('sf', '0.9.2')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('spData', '0.3.5')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('rgdal', '0.3.5')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('rmapshaper', '0.4.4')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('raster', '3.1-5')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('readxl', '1.3.1')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('rstudioapi', '0.13')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('httr', '1.4.2')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('purrr', '0.3.4')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('rvest', '0.3.5')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('tidyverse', '1.3.0')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('tidycensus', '0.9.9.5')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('lubridate', '1.7.9')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('stringr', '1.4.0')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('leaflet', '2.0.3')"

RUN R -e "install.packages('remotes'); \
  remotes::install_version('plotly', '4.9.2.1')"

CMD cd rcodes_state \
    && R -e "source('global0_set_apis_libraries.R')"









