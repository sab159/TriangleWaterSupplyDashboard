#Workflow for creating api

###########################################################################################################################
#
# Load Libaries
#
###########################################################################################################################
#library(httr) # for HTTP requests 
#library(jsonlite) # for parsing JSON from the SensorThings endpoint
#library(readxl) # for reading the XLSX templates
#library(tidyverse) # for data manipulation
#library(lubridate) # for datatime manipulation
#library(rstudioapi); #used to set working directory
#library(janitor); #for excel number formats to convert to date time

## First specify the packages of interest
packages = c("rstudioapi", "httr", "jsonlite", "readxl",
             "tidyverse", "lubridate", "janitor")

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


###########################################################################################################################
#
# SET UP GLOBAL VARIABLES
#
###########################################################################################################################
#set up path to save temporary files
source_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(source_path))


# endpoint <- "http://web:8080/FROST-Server/v1.1/" 
# This is the production endpoint for portability to a different environment, assuming a docker container named "web"
endpoint <- "https://twsd.internetofwater.dev/api/v1.1/" ; #This is the current pilot endpoint 

user <- "iow" # We will be changing these in production
pw <- "nieps" # We will be changing these in production

url_registry <- "https://raw.githubusercontent.com/internetofwater/TriangleWaterSupplyDashboard/master/utility_registry.csv"

registry <- read.csv(url_registry)



###########################################################################################################################
#
# Mapping TEMPLATE TO SENSOR OF THINGS DIRECTIONS
#
###########################################################################################################################
#SensorThings API has a particular data model that we need to conceptually map the template to before ingesting data. Our first draft of this dashboard is essentially mapping 4 basic pieces of data from utilities:
  # Utility Metadata, such as its name, PWSID, location, and contact information, updated only as needed or annually. (template sheet `system_metadata`)
  # Finished water deliveries at a daily timestep, updated weekly (template sheet `delivery`)
  # Supply conditions at a daily timestep, updated weekly (generally available water storage capacity from relevant reservoirs). (template sheet `supply_conditions`)
  # Active conservation status and associated policies, updated as needed. (template sheet `conservation_status`)

# Each Utility shall be a `Thing`, identified viw the U.S. EPA SDWIS PWSID.
  #Each `Thing` shall be associated with 1 `Location`, which is its Service Area Boundary or a centroid
    #Each `Thing` shall be associated with 2-3 `Datastream`s
      #The `Datastreams` are:
        #Finished water deliveries ("[PWSID]-WaterDistributed")
          #`Sensor`: Report of delivered water ("DemandReport")
          #`ObservedProperty`: Water distributed for use by consumers of utility water ("WaterDistributed")
          #`unitOfMeasurement`: "Million Gallons per Day (MGD)"
      #Water conservation status ("[PWSID]-ConservationStatus")
        #`Sensor`: Water Shortage Status form ("StageReport")
        #`ObservedProperty`: Phase of water shortage severity associated with appropriate responses for each phase ("ConservationStatus")
        #`unitOfMeasurement`: "Status"
      #Storage Capacity ("[PWSID]-StorageCapacity")
        #`Sensor`: Water Shortage Status form ("StorageReport")
        #`ObservedProperty`: Percent of storage capacity available for distribution ("StorageCapacity")
        #`unitOfMeasurement`: "Percent"
        #'featureofInterest: "Location"


#For each row in registry download the template and upload any NEW data to the sensor of things endpoing. If the PWSID is not present then add all
#If we receive HTTP status 200 it already exists. If we receive HTTP status 404 it does not. 

#####################################################################################################################################
# We write a convenience function to check a pwsid against an endpoint variable we have already set
staThingCheck <- function(api_url){
  call <- paste0(api_url,"Things?$select=@iot.id") 
  # This makes "https://example-sta-api_url.com/endpoint-interface/Things('{id}')
  response <- jsonlite::fromJSON(call)$value$`@iot.id`
  pwsids_in_sta <- registry$pwsid[which(registry$pwsid %in% response)]
  pwsids_not_in_sta <- registry$pwsid[which(!(registry$pwsid %in% response))]
  
  pwsids <- list("pwsids_in_sta"=pwsids_in_sta,"pwsids_not_in_sta"=pwsids_not_in_sta)
  return(pwsids)
}
pwsids = staThingCheck(endpoint)


######################################################################################################################################
# Create the Thing (utility metadata object) and its associated location. #we first must read in the entire excel template. 
#The function below defines this, given a path (url) to a an XSLX template, we can read in the template and import a list of dataframes corresponding to the XLSX sheets into the R environment. 
#*Note that this requires a subdirectory called `tmp` to exist in the working directory.*
 
readTemplate <- function(path) {
  httr::GET(path, httr::write_disk("tmp/tmp.xlsx", overwrite = TRUE))
  meta <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "system_metadata") %>% tibble::column_to_rownames(var = "field") %>% t() %>% as_tibble()
  
  sources <- readxl::read_excel("tmp/tmp.xlsx", sheet = "sources")
  monitoring_locations <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "monitoring_locations")
  conservation_policies <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "conservation_policies")
  delivery <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "delivery") %>% filter(begin_date != "YYYY-MM-DDTHH:MM:SS")
  supply <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "supply_conditions")
  monitoring_data <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "monitoring_data")
  conservation_status <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "conservation_status")
  
  unlink("tmp/tmp.xlsx")
  list <-
    list(
      metadata = meta,
      sources = sources,
      monitoring_locations = monitoring_locations,
      conservation_policies = conservation_policies,
      delivery = delivery,
      supply = supply,
      monitoring_data = monitoring_data,
      conservation_status = conservation_status
    )
  
  return(list)
}


#Here we use the function to read in the XLSX associated with the first row of the utility registry (Apex in this case).
path <- registry$data_url[1]
data <- readTemplate(path)
head(data)

#####################################################################################################################
# Metadata Sheet
#Now we can access the metadata sheet using `data$metadata` to create the Thing and Location into a hiearchical list object 
#that can be POSTed as JSON to the SensorThings API

meta <- data$metadata[1,]

#function
metaToUtilityThing <- function(meta) {
  id <- paste0("NC", gsub("-", "", meta$pwsid))
  service_area <- sf::read_sf(paste0("https://geoconnex.us/ref/pws/",id)) %>% sf::st_centroid()
  
  #convert to centroid to save space since calling in elsewhere
  #service_sfc <- sf::st_as_sfc(service_area);  #lp: this did not work for me
  thing <-
    list(
      `@iot.id` = id,
      name = meta$system_name,
      description = paste0("Data from the water utility: ",
                           meta$system_name),
      properties = list(
        county = meta$county,
        basin = meta$basin,
        ownership = meta$ownership,
        service_population = meta$service_population,
        contact_name = paste0(meta$contact_first_name, " ", meta$contact_last_name),
        contact_title = meta$contact_title,
        address = meta$address,
        city = meta$city,
        state = meta$state,
        zip = meta$zip,
        phone = meta$phone,
        fax = meta$fax,
        email = meta$email,
        wsrp_link = meta$wsrp_link
      )
    )
  
  loc <- list(
    `@iot.id` = paste0(id," - Service Area"),
    name = paste0(id, " Service area"),
    description = paste0("Service area of ",thing$name),
    encodingType = "application/vnd.geo+json",
    location = list(
      type="Point",
      coordinates = as.list(service_area$geometry[[1]])
      #coordinates = list(
      #  4.9,
      #  52.3
      #)
    )
  )
  utility <- list(thing=thing,location=loc)    
  return(utility)
}

meta <- metaToUtilityThing(meta)
str(meta)

#############################################################################################################################
#Finished water deliveries ("[PWSID]-WaterDistributed")
#`Sensor`: Report of delivered water ("DemandReport")
#`ObservedProperty`: Water distributed for use by consumers of utility water ("WaterDistributed")
#`unitOfMeasurement`: "Million Gallons per Day (MGD)"
deliv = data$delivery

#check dates
#remove first row if YYYY-MM-DDTHH:MM:SS
deliv = deliv %>% filter(substr(begin_date,1,4) != "YYYY")
dateFormat = deliv$begin_date[1]; dateFormatFinal = "check"

if(substr(dateFormat,5,5) == "-") {
  dateFormatFinal = "%Y-%m-%d"
  deliv = deliv %>% mutate(date = as.Date(as.character(begin_date), format=dateFormatFinal), 
                           end = as.Date(as.character(end_date), format=dateFormatFinal),
                           days = date-lag(date))
  }
if(substr(dateFormat,5,5) == "/") {
  dateFormatFinal = "%m/%d/%Y"
  deliv = deliv %>% mutate(date = as.Date(as.character(begin_date), format=dateFormatFinal),
                           end = as.Date(as.character(end_date), format=dateFormatFinal),
                           days = date-lag(date))
  }
if(as.numeric(dateFormat)>1000) {
  dateFormatFinal = "Excel"
  deliv = deliv %>% mutate(date = excel_numeric_to_date(as.numeric(as.character(begin_date)), date_system = "modern"),
                           end = excel_numeric_to_date(as.numeric(as.character(end_date)), date_system = "modern"),
                           days = as.numeric(date-lag(date)))
  }


#Now create the list
ds.deliver = list(
  id=paste(meta$thing$`@iot.id`,"WaterDistributed", sep="-"),
  name = paste0("Water distributed (MGD) by ", meta$thing$name),
  description = paste0("Average Water distributed (MGD) in the preceding period by ", meta$thing$name),
  observationType = "http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement",
  unit = list(definition="http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=Units&id=1125579048",
              name = "Million Gallons per Day",
              symbol = "MGD"),
  Thing_id = meta$thing$`@iot.id`,
  Sensor_id = "DemandReport",
  ObservedProperty_id = "WaterDistributed",
  phenomenonTime = list(
    paste0(deliv$date[1:10],"T00:00.000Z")
  #`@iot.id` = paste0(id,"-",phenomenonTime)
  )
)

deliver.obsv = list(
  `@iot.id` = paste0(ds.deliver$id,"-",deliv$date[1:10],"T00:00.000Z"),
  phenomenonTime = paste0(deliv$date[1:10],"T00:00.000Z"),
  parameters = list(
    `days in observed period` = deliv$days[1:10],
    result = deliv$delivery_million_gallons[1:10],
    resultTime = paste0(deliv$date[1:10],"T00:00:00.000Z")
  )
)

datastream.deliver <- list(datastream=ds.deliver, observation=deliver.obsv)    
#https://twsd.internetofwater.dev/api/v1.1/Datastreams('NC0332010-WaterDistributed')/Observations?$count=true
#Observations?value":[{"@iot.id":"NC0332010-WaterDistributed-2000-01-01T00:00:00.000Z",
                      #"phenomenonTime":"2000-01-01T00:00:00.000Z",
                      #"parameters":{"days in observed period":1},"result":25.411,"resultTime":"2000-01-01T00:00:00.000Z}]


#############################################################################################################################
#Conservation sheet
#Water conservation status ("[PWSID]-ConservationStatus")
#`Sensor`: Water Shortage Status form ("StageReport")
#`ObservedProperty`: Phase of water shortage severity associated with appropriate responses for each phase ("ConservationStatus")
#`unitOfMeasurement`: "Status"

consv = data$conservation_policies %>%  dplyr::select(-Description) %>% gather(key=activity, value=status_response, -conservation_status) %>% rename(status = conservation_status)

consv.now = data$conservation_status %>% filter(is.na(conservation_status)==FALSE) %>% mutate(date = today())
#assume similar date challenges
dateFormat = consv.now$date_activated[1]; dateFormatFinal = "check"
if(is.na(dateFormat)==FALSE) {
  if(substr(dateFormat,5,5) == "-") {
    dateFormatFinal = "%Y-%m-%d"
    consv.now = consv.now %>% mutate(date = as.Date(as.character(date_activated), format=dateFormatFinal))
  }
  if(substr(dateFormat,5,5) == "/") {
    dateFormatFinal = "%m/%d/%Y"
    consv.now = consv.now %>% mutate(date = as.Date(as.character(date_activated), format=dateFormatFinal))
  }
  if(as.numeric(dateFormat)>1000) {
    dateFormatFinal = "Excel"
    consv.now = consv.now %>% mutate(date = excel_numeric_to_date(as.numeric(as.character(date_activated)), date_system = "modern"))
  }
}
#if no date - set to present date. Did this at the beginning to avoid date errors with "ifelse"
consv.now <- consv.now %>% mutate(date = if_else(is.na(date_activated), today(), date)) 
consv.now = consv.now %>% filter(date == max(date))

ds.consv_table = list(
  id=paste(meta$thing$`@iot.id`,"ConservationStatus", sep="-"),
  name = paste0("Conservation Status and Activities by ", meta$thing$name),
  description = paste0("Activities allowed based on conservation status for ", meta$thing$name),
  unitOfMeasurement = "status",
  Thing_id = meta$thing$`@iot.id`,
  Sensor_id = "StageReport",
  ObservedProperty_id = "ConservationStatus",
  ObservedProperty = list(name = consv$status,
                          definition = consv$activity,
                          description = consv$status_response),
  Sensor = consv.now$conservation_status
  
)
ds.consv_table





#Now we have to HTTP POST this object to STA. First we create HTTP POST, PATCH, and some convenience functions to enable this.
staPost <- function(url, payload, user, password) {
  httr::POST(
    url = url,
    encode = "json",
    httr::authenticate(user, password, type = "basic"),
    body = payload
  )
}

staPatch <- function(url, payload, user, password) {
  httr::PATCH(
    url = url,
    encode = "json",
    httr::authenticate(user, password, type = "basic"),
    body = payload
  )
}

strip_iot_id <- function(list){
  list$`@iot.id` <- NULL
  return(list)
}


#With these convenience functions, we can now create a function that processes the entire sheet and uploads to STA.
uploadUtility <- function(api, utility, user, password) { 
  id <- utility$thing$`@iot.id`
  id.l <- utility$location$`@iot.id`
  
  status <- httr::GET(paste0(api, "Things('", id, "')"))$status
  if (status == 200) {
    
    staPatch(
      url = paste0(api, "Things('", id, "')"),
      payload = strip_iot_id(utility$thing),
      user = user,
      password = password
    )
    
    staPatch(
      url = paste0(api, "Locations('", id.l, "')"),
      payload = strip_iot_id(utility$location),
      user = user,
      password = password
    )
  } else {
    staPost(
      url = paste0(api, "Things"),
      payload = utility$thing,
      user = user,
      password = password
    )
    staPost(
      url = paste0(api, "Things('", id, "')/Locations"),
      payload = utility$location,
      user = user,
      password = password
    )
  }
}
uploadUtility(endpoint, meta, user, pw)
