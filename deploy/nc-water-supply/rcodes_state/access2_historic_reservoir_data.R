#######################################################################################################################################################
#
# Creates initial map layers and downloads historic data for the dashboard
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# FEBRUARY 2021
# Should not need to be run 
#
########################################################################################################################################################

######################################################################################################################################################################
#
#   LOAD OLD DATA PREVIOUSLY COLLECTED BY THE WATER PROGRAM
#
######################################################################################################################################################################
#This old data includes updates over time
old.data <- read.csv(paste0(swd_html, "reservoirs\\usace_dams.csv"))



######################################################################################################################################################################
#
#   ONLY NEED THIS CODE IF THE CORPS UPDATES THEIR API - NEED TO FIND NEW URLS
#
######################################################################################################################################################################
baseURL = 'https://water.usace.army.mil/a2w/'
# This is loads in the locations and the urls for other data accessible via web services
projectsURL = 'https://water.usace.army.mil/a2w/CWMS_CRREL.cwms_data_api.get_a2w_webservices_json'

#read in data as json file
project.data = jsonlite::fromJSON(projectsURL, simplifyDataFrame=FALSE)
#project.data <- readLines(projectsURL, warn=FALSE)
  #fix error
  #project.data <- gsub("},]}", "}]}", project.data)
  #project.data <- jsonlite::fromJSON(project.data, simplifyDataFrame=FALSE)
  str(project.data, max.level=1)
  #length of lists
  nprojects <- length(project.data$projects)
  
  #to see levels
  project.data$projects[[1]]
  
#To pull out individual data frames and find data URL for individual reservoirs
project.df = as.data.frame(matrix(nrow=nprojects, ncol=10));   
  colnames(project.df) <- c("District", "Lat_Y", "Long_X", "Loc_ID", "Name", "DataURL", "DamSummaryURL","WQReportURL","LocSummaryURL","AnnualVarURL")
  for (i in 1:nprojects){
    project.df$District[i] = project.data$projects[[i]]$office_id
    project.df$Lat_Y[i] = as.numeric(project.data$projects[[i]]$lat)
    project.df$Long_X[i] = as.numeric(project.data$projects[[i]]$lon)
    project.df$Loc_ID[i] = project.data$projects[[i]]$loc_id
    project.df$Name[i] = project.data$projects[[i]]$description
    project.df$DataURL[i] = paste0(baseURL, project.data$projects[[i]]$reportURL)
    project.df$DamSummaryURL[i] = paste0(baseURL, project.data$projects[[i]]$damSummaryURL)
    project.df$WQReportURL[i] = paste0(baseURL, project.data$projects[[i]]$wqReportURL)
    project.df$LocSummaryURL[i] = paste0(baseURL, project.data$projects[[i]]$locSummaryURL)
    project.df$AnnualVarURL[i] = paste0(baseURL, project.data$projects[[i]]$annualVariabilityURL)
  }
project.df[1:5,]
table(project.df$District)

#create shapefile of data
dams <- st_as_sf(project.df, coords = c("Long_X", "Lat_Y"), crs = 4326, agr = "constant") %>% filter(District=="SAW")
mapview::mapview(dams)



##########################################################################################################################################################################  
#The data collected by Duke uses NIDID while the Corps uses Loc_ID - so we had to create a match file
project.df <- read.csv(paste0(swd_html, "reservoirs\\matchNID_LocID.csv"))


#full_url <- "https://water.usace.army.mil/a2w/CWMS_CRREL.cwms_data_api.get_report_json?p_location_id=1745041&p_parameter_type=Stor%3AElev&p_start_date=2015-09-01&p_end_date=2016-08-31&p_unit_system=EN&p_format=CSV"
last_number = 1; #number of units to collect
last_unit = "years"; #other options are months, weeks, and days
report_url = "CWMS_CRREL.cwms_data_api.get_report_json?p_location_id="
parameter_url <- paste0("&p_parameter_type=Stor%3AElev&p_last=", last_number, "&p_last_unit=", last_unit, "&p_unit_system=EN&p_format=JSON")

#create dataframes
district.id = "SAW"
district_data <- as.data.frame(matrix(nrow=0, ncol=8)); colnames(district_data) <- c("date", "elev_Ft", "storage_AF", "fstorage_AF", "locid", "district", "NIDID", "name")
zt <- subset(project.df, District==district.id)

for (i in 1:length(zt$Loc_ID)){
  location.id <- zt$Loc_ID[i]; location.id

  full_url <- paste0(baseURL, report_url, location.id, parameter_url)

  api.data <- GET(full_url, timeout(15000)) #use httr libirary to avoid timeout
  dam.data <- content(api.data, "parse")
  
  #In future iterations - this is where cleaning should occur. They have normal numbers mixed in with very small numbers. We'd want to remove those to get a better "median" value.
  lake_level = data_frame(
    time = map_chr(dam.data[[1]]$Elev, "time"),
    elev_FT = map_dbl(dam.data[[1]]$Elev, "value")
  )
  
  #lake_level$date = dmy_hms(lake_level$time, tz="EST")
  lake_level$date <- as.Date(substr(lake_level$time,1,11), "%d-%b-%Y")
  lake_level <- lake_level %>% group_by(date) %>% summarize(elev_Ft = round(median(elev_FT, na.rm=TRUE),2), .groups = "drop")
  plot(lake_level$date, lake_level$elev_Ft, type="l"); #abline(h=251.5, col="blue")
  
  #check to see if storage exists - because sometimes doesn't, build dataframe separately
  #the list number changes depending on what is available
  if (dim(lake_level)[1] > 0){
    for(j in 2:3){
      time1 = map_chr(dam.data[[j]]$`Flood Storage`, "time");
      storage_AF1 = map_int(dam.data[[3]]$`Conservation Storage`, "value");
      storf1 = map_int(dam.data[[j]]$`Flood Storage`, "value");
      
      if (length(time1) > 0){ 
        time = time1; 
        storage_AF = storage_AF1;
        fstor_AF = storf1; 
      }
    } #end data fill
  } #end lake level
    
  if (dim(lake_level)[1] == 0){
    for(j in 1:2){
      #lake_stor = data_frame(
      time1 = map_chr(dam.data[[j]]$`Flood Storage`, "time");
      storage_AF1 = map_int(dam.data[[3]]$`Conservation Storage`, "value");
      storf1 = map_int(dam.data[[j]]$`Flood Storage`, "value");
      if (length(time1) > 0){
        if (length(time1) > 0){ 
          time = time1; 
          storage_AF = storage_AF1;
          fstor_AF = storf1; 
        }
      }
    }
  }#end lake level

  lake_stor <- cbind(time, storage_AF, fstor_AF) %>% as.data.frame()
  #Split time and group
  lake_stor$date <- as.Date(substr(lake_stor$time,1,11), "%d-%b-%Y")
  lake_stor <- lake_stor %>% group_by(date) %>% summarize(storage_AF = round(median(as.numeric(as.character(storage_AF)), na.rm=TRUE),0), fstorage_AF = round(median(as.numeric(as.character(fstor_AF)), na.rm=TRUE),0), .groups="drop")
  plot(lake_stor$date, lake_stor$storage_AF, type="l"); lines(lake_stor$date, lake_stor$fstorage_AF, col="red")
  
  lake_data <- merge(lake_level, lake_stor, by.x="date", by.y="date", all=TRUE)
  lake_data$locid <- as.character(location.id);     lake_data$district <- district.id;
  lake_data$NIDID <- as.character(zt$NIDID[i]);     lake_data$name <- as.character(zt$Name[i])
  
  #bind to larger dataframe
  #if no data
  district_data <- rbind(district_data, lake_data)
  rm(api.data)
  print(paste0(location.id,": ", as.character(zt$Name[i])))
}
summary(district_data);

#Now read in the website data and add the most recent files to the end of it
new.data <- district_data
res <- zt %>% select(District, Name, NIDID, Lat_Y, Long_X, Loc_ID)

#pull out unique reservoirs
unique.nid <- unique(new.data$NIDID); unique.nid
############################################################################################################
#new data
  nx <- new.data 
    dateFormat = nx$date[1]
      if(substr(dateFormat,5,5) == "-") {dateFormatFinal = "%Y-%m-%d"}
      if(substr(dateFormat,5,5) != "-") {dateFormatFinal = "%m/%d/%Y"}
    dateFormatFinal
  nx$date <- as.Date(as.character(nx$date), dateFormatFinal) 
  nx$Year <- year(nx$date)
  nx$day_month <- substr(nx$date, 6, 10)
  nx$julian <-  as.integer(julian(nx$date))%%365L+1L
  
  #clean data
  nx <- nx %>% mutate(elev_Ft = ifelse(elev_Ft <= 0, NA, elev_Ft), storage_AF = ifelse(storage_AF <=0, NA, storage_AF), fstorage_AF = ifelse(fstorage_AF <=0, NA, fstorage_AF))
    maxCap <- nx %>% group_by(NIDID) %>% summarize(maxCap = 1.2*quantile(elev_Ft, 0.90, na.rm=TRUE), maxStor = 1.2*quantile(storage_AF, 0.90, na.rm=TRUE), maxfStor = 1.2*quantile(fstorage_AF, 0.90, na.rm=TRUE), .groups="drop");
  nx <- nx %>% left_join(maxCap, by="NIDID") %>% mutate(elev_Ft = ifelse(elev_Ft > maxCap, NA, elev_Ft), storage_AF = ifelse(storage_AF > maxStor, NA, storage_AF), fstorage_AF = ifelse(fstorage_AF > maxfStor, NA, fstorage_AF)) %>% 
      select(-maxCap, -maxStor, -maxfStor)
    
  #old data
  fx <- old.data
  dateFormat = fx$DATE[1]
  if(substr(dateFormat,5,5) == "-") {dateFormatFinal = "%Y-%m-%d"}
  if(substr(dateFormat,5,5) != "-") {dateFormatFinal = "%m/%d/%Y"}
  fx$date <- as.Date(as.character(fx$DATE), dateFormatFinal) 
  fx$Year <- year(fx$date)
  fx$day_month <- substr(fx$date, 6, 10)
  fx <- fx %>% select(NIDID, Name, date, Year, day_month, JULIAN, Elev_Ft, Storage_ACFT, OT_FT, OT_ACFT) %>% rename(julian = JULIAN)
  
  #what is the most recent date?
  old.last.date <- fx %>% group_by(NIDID) %>% filter(date == max(date, na.rm=TRUE)) %>% select(NIDID, date) %>% distinct() %>% rename(lastDate = date)
  
  #remove anything new before that date
  nx2 <- nx %>% left_join(old.last.date, by="NIDID") %>% filter(date > lastDate)
  fx.2014 <- fx %>% filter(Year==2012) %>% select(NIDID, julian, OT_FT, OT_ACFT); #delete by 8 because 2016 data are incomplete
  nx2 <- merge(nx, fx.2014, by.x=c("NIDID","julian"), by.y=c("NIDID","julian"), all.x=TRUE)  
  
  nx2 <- nx2 %>% select(NIDID, name, date, Year, day_month, julian, elev_Ft, storage_AF, OT_FT, OT_ACFT); colnames(nx2) <- colnames(fx)
  
  #combine
  fx <- rbind(fx, nx2)
  
  #make sure no duplicates
  fx <- fx %>% distinct()
  #arrange by NIDID and date
  fx <- fx %>% arrange(NIDID, date)
  
  fx <- fx %>% mutate(percentStorage = round(Storage_ACFT/OT_ACFT*100,2)) %>% mutate(Storage_ACFT = ifelse(percentStorage > 300, NA, Storage_ACFT), percentStorage = ifelse(percentStorage > 300, NA, percentStorage))
  summary(fx)
  write.csv(fx, paste0(swd_html, "reservoirs\\usace_dams.csv"), row.names=FALSE)

  #save shapefile
  zt <- zt %>% select(District, Name, NIDID, Lat_Y, Long_X, Loc_ID)
  res.loc <- st_as_sf(zt, coords = c("Long_X", "Lat_Y"), crs = 4326);
  geojson_write(res.loc, file=paste0(swd_html, "reservoirs//usace_sites.geojson"))
########################################################################################################################################################################################

  
  




