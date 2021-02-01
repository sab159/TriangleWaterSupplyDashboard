# Deployment

This directory will house a complete docker-based orchestration (docker-compose.yml) for resources necessary to run the Triangle Water Supply Dashboard, including:

1. A rocker (Ubuntu-based R image) container that
 - a. Includes all necessary packages with a fixed R version and package versions
 - b. Schedules runs of R scripts that:
   -  i. Import data from utility-submitted templates to the included SensorThings API. 
   -  ii. Download data from the the included SensorThings API and publicly Available sources and produces the Water Supply Dashboard
   
   Rscripts should be stored in [/src](/deploy/src) and their schedules specified in [src/cronjob](/deploy/src/cronjob). 
   
2. A FROST implementation of SensorThings API

3. A Postgres database storing the data served by SensorThings API

4. A Caddy web server directing web traffic to the Dashboard or the API depending on the request

5. A storage volume allowing Caddy, Postgres, and rocker to share resources with each other, mapped to a directory on the host machine for manual manipulation and backup

    
    
