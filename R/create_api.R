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
package.check <- function(packages){lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
}

package.check(packages)

### Create functions to interact with STA
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

PostObservedProperty <- function(api,user,password,id,name,definition,description){
  payload = list("@iot.id"=id,
                 "name"=name,
                 "definition"=definition,
                 "description"=description)
  staPost(paste0(api,"ObservedProperties"),
          payload,
          user,
          password)
}


PostSensor <- function(api,user,password,id,name,description,encodingType,metadata){
  payload = list("@iot.id"=id,
                 "name"=name,
                 "description"=description,
                 "encodingType"=encodingType,
                 "metadata"=metadata)
  staPost(paste0(api,"Sensors"),
          payload,
          user,
          password)
}

# Define Observed Properties
op <- PostObservedProperty(api=endpoint, user, pw,
                           id = "WaterDistributed",
                           name = "Water Distributed",
                           definition = "http://vocabulary.odm2.org/api/v1/variablename/waterUsePublicSupply/",
                           description = "Water distributed for use by consumers of utility water")

op <- PostObservedProperty(api=endpoint, user, pw,
                           id = "StorageCapacity",
                           name = "Storage Capacity (% full)",
                           definition = "http://vocabulary.odm2.org/api/v1/variablename/reservoirStorage/",
                           description = "Percentage of water storage capacity available for distribution")

op <- PostObservedProperty(api=endpoint, user, pw,
                           id = "WaterShortageStage",
                           name = "Water Shortage Stage",
                           definition = "https://www.ncwater.org/WUDC/app/LWSP/learn.php",
                           description = "Phases of water shortage severity associated with appropriate responses for each phase")

# Define Sensors
s <- PostSensor(api=endpoint, user, pw,
                id = "ShortageStageReport",
                name = "Water Shortage Status",
                description = "Water Shortage Status form submitted to ncwater.org",
                encodingType = "application/pdf",
                metadata = "https://www.ncwater.org/WUDC/")

s <- PostSensor(api=endpoint, user, pw,
                id = "StorageReport",
                name = "Report of available water storage",
                description = "Report of available water storage",
                encodingType = "application/pdf",
                metadata = "https://www.ncwater.org/WUDC/")

s <- PostSensor(api=endpoint, user, pw,
                id = "DemandReport",
                name = "Report of delivered water",
                description = "Report of delivered water",
                encodingType = "application/pdf",
                metadata = "https://www.ncwater.org")


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
endpoint <- "https://twsd.internetofwater.dev/api/v1.1/"  #This is the current pilot endpoint 

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
path <- registry$data_url[5]
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

#why does location fit in there even though it should go to it's own spot?
staPost(paste0(endpoint,"Things"), meta, user, pw )



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


#Now create the list ****ONLY DOING 10 OBSERVATIONS UNTIL KNOW IT IS CORRECT ******
ds.deliver = list(
  `@iot.id`=paste(meta$thing$`@iot.id`,"WaterDistributed", sep="-"),
  name = paste0("Water distributed (MGD) by ", meta$thing$name),
  description = paste0("Average Water distributed (MGD) in the preceding period by ", meta$thing$name),
  observationType = "http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement",
  unitOfMeasurement = list(definition="http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=Units&id=1125579048",
              name = "Million Gallons per Day",
              symbol = "MGD"),
  thing_id = list(`@iot.id`=meta$thing$`@iot.id`), #this line breaks it
  Sensor = list(`@iot.id`="DemandReport"),
  ObservedProperty = list(`@iot.id`="WaterDistributed")
  
)
jsonlite::toJSON(ds.deliver, auto_unbox=TRUE)
#check to see if works... it does
staPost(paste0(endpoint,"Datastreams"), ds.deliver, user, pw )

deliv$`@iot.id` <- paste0(ds.deliver$`@iot.id`,"-",deliv$date,"T00:00.000Z")
deliv$phenomenonTime <- paste0(deliv$date,"T00:00.000Z")
deliv$result <- deliv$delivery_million_gallons


deliv.obsv = list(
  `@iot.id` = deliv$`@iot.id`[1],
  phenomenonTime = deliv$phenomenonTime[1],
  result = deliv$result[1],
  resultTime = deliv$phenomenonTime[1]
)
#for (i in 2:length(deliv$result)){
for (i in 2:10){
  zt <- list(
    `@iot.id` = deliv$`@iot.id`[i],
    phenomenonTime = deliv$phenomenonTime[i],
    result = deliv$result[i],
    resultTime = deliv$phenomenonTime[i]
  )
  deliv.obsv = list(deliv.obsv, zt)
}
ds = list(datastream = ds.deliver, observation=deliv.obsv)    
jsonlite::toJSON(ds, auto_unbox=TRUE); #check lists
staPost(paste0(endpoint,"Datastreams"), ds, user, pw )


#https://twsd.internetofwater.dev/api/v1.1/Datastreams('NC0332010-WaterDistributed')/Observations?$count=true
#Observations?value":[{"@iot.id":"NC0332010-WaterDistributed-2000-01-01T00:00:00.000Z",
                      #"phenomenonTime":"2000-01-01T00:00:00.000Z",
                      #"parameters":{"days in observed period":1},"result":25.411,"resultTime":"2000-01-01T00:00:00.000Z}]
#plot(as.Date(datastream.deliver$observation$parameters$resultTime, format="%Y-%m-%d"), datastream.deliver$observation$parameters$result, type="l")

#############################################################################################################################
#Conservation sheet
#Water conservation status ("[PWSID]-ConservationStatus")
#`Sensor`: Water Shortage Status form ("StageReport")
#`ObservedProperty`: Phase of water shortage severity associated with appropriate responses for each phase ("ConservationStatus")
#`unitOfMeasurement`: "Status"

#column names not the same between Apex and Durham... trying to fix...
consv = data$conservation_policies[,-2]
consv <- consv %>% rename(conservation_status = colnames(consv[,1])) %>% 
  gather(key=activity, value=status_response, -conservation_status) %>% rename(status = conservation_status)
#if left the example column - remove
consv <- consv %>% filter(is.na(status_response)==FALSE)

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
  if(as.numeric(dateFormat)>1000 & as.numeric(dateFormat)<6000) {
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
  thing_id = meta$thing$`@iot.id`,
  Sensor_id = "StageReport",
  ObservedProperty_id = "ConservationStatus",
  ObservedProperty = list(name = consv$status,
                          definition = consv$activity,
                          description = consv$status_response),
  Sensor = consv.now$conservation_status
)
str(ds.consv_table)
ds.consv = list(DataStream = ds.consv_table)    
jsonlite::toJSON(ds.consv, auto_unbox=TRUE); #check lists
staPost(paste0(endpoint,"Datastreams"), ds.consv, user, pw )



#############################################################################################################################
# Storage Capacity Sheet
#Storage Capacity ("[PWSID]-StorageCapacity")
#`Sensor`: Water Shortage Status form ("StorageReport")
#`ObservedProperty`: Percent of storage capacity available for distribution ("StorageCapacity")
#`unitOfMeasurement`: "Percent"
#'featureofInterest: "Location"

store <- data$supply %>% filter(substr(date,1,4) != "YYYY")

#storage locations will be part of the utility shapefile... does not look like attached to monitoring location
#mon.loc <- data$monitoring_locations; #Durham provided storage volume but not a location... should we use centroid of utility then?
#mon.loc <- mon.loc %>% filter(is.na(as.numeric(latitude)) == FALSE) %>% dplyr::select(monitoring_location_name, parameters, latitude, longitude)
#if (dim(mon.loc)[1] == 0){

#how many sources present?
  store.type <- unique(store[,c("source_name", "source_type")])
#create location dataframe
  store.loc <- as.data.frame(matrix(nrow=dim(store.type)[1], ncol=4)); colnames(store.loc) <- c("name", "store_type", "latitude", "longitude")
  for(i in 1:dim(store.type)[1]){
    store.loc$name[i]  = store.type$source_name[i]; 
    store.loc$store_type[i] = store.type$source_type[i];
    store.loc$latitude[i] = round(meta$location$location$coordinates[[1]] + 0.015*i,5); #add some jitter in case several
    store.loc$longitude[i] = round(meta$location$location$coordinates[[2]] + 0.015*i,5); #add some jitter in case serveral
  }
store.loc

#initialize list
featureList = list(
  name = store.loc$name[1],
  type="Point",
  coordinates = list(store.loc$latitude[1], store.loc$longitude[1])
)

for(i in 2:length(store.loc$name)){
  zt = list(
    name = store.loc$name[i],
    type="Point",
    coordinates = list(store.loc$latitude[i], store.loc$longitude[i])
  )
  featureList = list(featureList, zt)
}
jsonlite::toJSON(featureList)



#now fix store dates-------------------------
dateFormat = store$date[1]; dateFormatFinal = "check"
if(substr(dateFormat,5,5) == "-") {
  dateFormatFinal = "%Y-%m-%d"
  store = store %>% mutate(date = as.Date(as.character(date), format=dateFormatFinal))
}
if(substr(dateFormat,5,5) == "/") {
  dateFormatFinal = "%m/%d/%Y"
  store = store %>% mutate(date = as.Date(as.character(date), format=dateFormatFinal))
}
if(as.numeric(dateFormat)>1000) {
  dateFormatFinal = "Excel"
  store = store %>% mutate(date = excel_numeric_to_date(as.numeric(as.character(date)), date_system = "modern"))
}

#Note that part way through the data for Durham the "source_metric" and "source_unit" were no longer filled out.
#leaving as NA for now... do we want to try to correct errors or have state go back and have data entrants fix errors? I think the latter.
#convert percent_full to value between 0 and 100
store <- store %>% mutate(value = ifelse(source_type=="reservoir" & value <= 5, value*100, value))

ds.store = list(
  id=paste(meta$thing$`@iot.id`,"StorageCapacity", sep="-"),
  name = paste0("Monitoring Data provided by ", meta$thing$name),
  description = paste0("Storage capacity available for distribution (reservoir, stream, groundwater) for ", meta$thing$name),
  observationType = "http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement",
  unit = list(definition="http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=Units&id=1125579048",
              list(
                name = "Percent Full",
                symbol = "%"),
              list(
                name = "Groundwater Level in Feet",
                symbol = "FT"),
              list(
                name = "Streamflow Level",
                symbol = "CFS")
              ),
  thing_id = meta$thing$`@iot.id`,
  Sensor_id = "StorageReport",
  ObservedProperty_id = "StorageCapacity"
) # end dstore list
staPost(paste0(endpoint,"Datastreams"), ds.store, user, pw )


#Now create the list ****ONLY DOING 10 OBSERVATIONS UNTIL KNOW IT IS CORRECT ******
zt.store <- store.loc %>% filter(name==store$source_name[1])
store.obsv = list(
  `@iot.id` = paste0(ds.store$id,"-",store$date[1],"T00:00.000Z"),
  phenomenonTime = paste0(store$date[1],"T00:00.000Z"),
  result = store$value[1],
  resultTime = paste0(store$date[1],"T00:00.000Z"),
  parameters = list(
    name = store$source_name[1],
    type = store$source_type[1],
    unit = store$source_unit[1]
  ), #end parameters list
  FeaturesofInterest = list(
    description = "Location of Storage Capacity Measurements for Utility",
    encodingType = "application/vnd.geo+json",
    feature = list(
      name = zt.store$name[1],
      type="Point",
      coordinates = list(zt.store$latitude[1], zt.store$longitude[1])
    )
  )
)
datastream.store = list(datastream = ds.store, observation=store.obsv)    
staPost(paste0(endpoint,"Datastreams"), ds.store, user, pw )

#for (i in 2:length(deliv$result)){
for (i in 2:10){
 zt.store <- store.loc %>% filter(name==store$source_name[i])
  zt = list(
    `@iot.id` = paste0(ds.store$id,"-",store$date[i],"T00:00.000Z"),
    phenomenonTime = paste0(store$date[i],"T00:00.000Z"),
    result = store$value[i],
    resultTime = paste0(store$date[i],"T00:00.000Z"),
    parameters = list(
      name = store$source_name[i],
      type = store$source_type[i],
      unit = store$source_unit[i]
    ), #end parameters list
    FeaturesofInterest = list(
      description = "Location of Storage Capacity Measurements for Utility",
      encodingType = "application/vnd.geo+json",
      feature = list(
        name = zt.store$name[i],
        type="Point",
        coordinates = list(zt.store$latitude[1], zt.store$longitude[1])
      )
    )
  ) #end observation list
store.obsv = list(store.obsv, zt)
}
datastream.store = list(datastream = ds.store, observation=store.obsv)    
staPost(paste0(endpoint,"Datastreams"), ds.store, user, pw )

#plot(as.Date(ds.store$observation$parameters$resultTime, format="%Y-%m-%d"), ds.store$observation$parameters$result, type="l")






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
