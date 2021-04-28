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
library(rstudioapi)
library(readxl); #in tidyverse
library(sf); library(rgdal); library(spData); library(raster); library(leaflet);
library(tidycensus); #requires a personal api
library(jsonlite); library(tidyverse); library(lubridate)
library(rmapshaper); library(geojsonio)
library(rvest); library(purrr); library(stringr);  library(httr); 

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


#census api key - 
census_api_key("95ed45e11e89f232bfc54d2541f31858c8cfbf9e", install=TRUE, overwrite=TRUE); #LAUREN REMEMBER TO DELETE THIS
readRenviron("~/.Renviron")


#nc state climate office key
ncsco.key <- "43977148bc44a20e61d3aaea3b95f161c1f56726"; #WHOEVER INHERITS WILL NEED TO CREATE THEIR OWN KEY AND PUT IT HERE

#load data used throughout
julian <- read.csv(paste0(swd_html, "julian-daymonth.csv"))

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



