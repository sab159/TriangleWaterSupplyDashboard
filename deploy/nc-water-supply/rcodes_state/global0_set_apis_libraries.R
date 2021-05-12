#######################################################################################################################################################
#
# DOWNLOADS AND CREATES GEOJSON FILES FOR MAP LAYERS IN THE NORTH CAROLINA WATER SUPPLY DASHBOARD
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# FEBRUARY 2021
#
########################################################################################################################################################

######################################################################################################################################################################
#
#   LOAD LIBRARIES
#
######################################################################################################################################################################
## First specify the packages of interest
packages = c("rstudioapi", "readxl", 
             "sf", "rgdal", "spData", "raster", "leaflet", "rmapshaper","geojsonio",
             "tidycensus", "jsonlite", "rvest", "purrr", "httr",
             "tidyverse", "lubridate", "plotly", "stringr")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#usgs packages
install.packages("dataRetrieval", repos=c("http://owi.usgs.gov/R", getOption("repos")))
library(dataRetrieval);  library(EGRET); #usgs links

######################################################################################################################################################################


######################################################################################################################################################################
#
#   SET GLOBAL VARIABLES
#
######################################################################################################################################################################
options(scipen=999) #changes scientific notation to numeric
rm(list=ls()) #removes anything stored in memory

#set working directory
source_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(source_path))
swd_html <- paste0("..\\data_state\\")

#state info
stateAbb <- "NC"
stateFips <- 37


#nc state climate office key - https://api.climate.ncsu.edu/
ncsco.key <- "43977148bc44a20e61d3aaea3b95f161c1f56726"; #WHOEVER INHERITS WILL NEED TO CREATE THEIR OWN KEY AND PUT IT HERE


#load data used throughout
#julian <- read.csv(paste0(swd_html, "julian-daymonth.csv"))

#variables used to update data
today = substr(Sys.time(),1,10); today;
current.year <- year(today);

start.date = "1990-01-01"; #set for the start of the period that we want to assess
end.date = paste0(year(today), "-12-31")
end.year = year(Sys.time())

mymonths <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"); #used below to convert numbers to abbrev

#save out update date for dashboard
update.date <- paste0(mymonths[month(today)]," ", day(today), ", ", end.year) %>% as.data.frame()
colnames(update.date) <- "today_date"
write.csv(update.date, paste0(swd_html, "update_date.csv"), row.names=FALSE)

#calculate moving average function
ma <- function(x,n=7){stats::filter(x,rep(1/n,n), sides=1)}



