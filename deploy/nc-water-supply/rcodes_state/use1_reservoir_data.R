#######################################################################################################################################################
#
# Updates reservoir  data based on historic data collected
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# FEBRUARY 2021
# Can update anytime
#
########################################################################################################################################################

######################################################################################################################################################################
#
#   LOAD Old Data - make sure parameters to update don't miss any data
#
######################################################################################################################################################################
#Now read in the website data and add the most recent files to the end of it
old.data <- read.csv(paste0(swd_html, "reservoirs\\usace_dams.csv"))
max(old.data$date)
timeAmt = 1
timeUnit = "months"

######################################################################################################################################################################
#
#   UPDATE WITH RECENT DATA
#
######################################################################################################################################################################
#set up url
baseURL = 'https://water.usace.army.mil/a2w/'
report_url = "CWMS_CRREL.cwms_data_api.get_report_json?p_location_id="
parameter_url <- paste0("&p_parameter_type=Stor%3AElev&p_last=", timeAmt, "&p_last_unit=", timeUnit, "&p_unit_system=EN&p_format=JSON");   #when weekly change months to weeks

#read in shapefile
project.df <- read_sf(paste0(swd_html, "reservoirs\\usace_sites.geojson"))
#add url to data
res.url <- "http://water.usace.army.mil/a2w/f?p=100:1:0::::P1_LINK:"
project.df <- project.df %>% mutate(url_link = paste0(res.url,Loc_ID,"-CWMS"))

#create dataframes
district_data <- as.data.frame(matrix(nrow=0, ncol=8)); colnames(district_data) <- c("date", "elev_Ft", "stroage_AF", "fstorage_AF", "locid", "district", "NIDID", "name")

for (i in 1:length(project.df$Loc_ID)){
  location.id <- project.df$Loc_ID[i]; location.id
  full_url <- paste0(baseURL, report_url, location.id, parameter_url)

  api.data <- GET(full_url, timeout(15000)) #use httr library to avoid timeout
    dam.data <- content(api.data, "parse")

  lake_level = data_frame(
    time = map_chr(dam.data[[1]]$Elev, "time"),
    elev_FT = map_dbl(dam.data[[1]]$Elev, "value")
  )
  
  lake_level <- lake_level %>% mutate(date = as.Date(substr(lake_level$time,1,11), "%d-%b-%Y")) %>% group_by(date) %>% summarize(elev_Ft = round(median(elev_FT, na.rm=TRUE),2), .groups = "drop")
  #plot(lake_level$date, lake_level$elev_Ft, type="l"); #abline(h=251.5, col="blue")
  
  #check to see if storage exists - because sometimes doesn't, build dataframe separately... the list number changes depending on what is available
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
  lake_stor <- lake_stor %>% mutate(date = as.Date(substr(lake_stor$time,1,11), "%d-%b-%Y")) %>% group_by(date) %>% 
           summarize(storage_AF = round(median(as.numeric(as.character(storage_AF)), na.rm=TRUE),0), fstorage_AF = round(median(as.numeric(as.character(fstor_AF)), na.rm=TRUE),0), .groups="drop")
  #plot(lake_stor$date, lake_stor$storage_AF, type="l"); lines(lake_stor$date, lake_stor$fstorage_AF, col="red")
  
  lake_data <- merge(lake_level, lake_stor, by.x="date", by.y="date", all=TRUE)
  lake_data$locid <- as.character(location.id);             lake_data$district <- "SAW";
  lake_data$NIDID <- as.character(project.df$NIDID[i]);     lake_data$name <- as.character(project.df$Name[i])
  
  #bind to larger dataframe
  district_data <- rbind(district_data, lake_data)
  rm(api.data)
  print(paste0(location.id,": ", as.character(project.df$Name[i])))
}
summary(district_data);


new.data <- district_data
########################################################################################################################################################################################################################
#
#          ADD OLD AND NEW DATA TOGETHER
#
########################################################################################################################################################################################################################
#pull out unique reservoirs
unique.nid <- unique(new.data$NIDID); unique.nid

#new data
  nx <- new.data 
    dateFormat = nx$date[1]
      if(substr(dateFormat,5,5) == "-") {dateFormatFinal = "%Y-%m-%d"}
      if(substr(dateFormat,5,5) != "-") {dateFormatFinal = "%m/%d/%Y"}
    dateFormatFinal
  nx$date <- as.Date(as.character(nx$date), dateFormatFinal) 
  nx$Year <- year(nx$date)
  nx$day_month <- substr(nx$date, 6, 10)
  nx$julian <-  as.integer(format(nx$date, "%j"))
  
  #clean data
  nx <- nx %>% mutate(elev_Ft = ifelse(elev_Ft <= 0, NA, elev_Ft), storage_AF = ifelse(storage_AF <=0, NA, storage_AF), fstorage_AF = ifelse(fstorage_AF <=0, NA, fstorage_AF))
    maxCap <- nx %>% group_by(NIDID) %>% summarize(maxCap = 1.2*quantile(elev_Ft, 0.90, na.rm=TRUE), maxStor = 1.2*quantile(storage_AF, 0.90, na.rm=TRUE), maxfStor = 1.2*quantile(fstorage_AF, 0.90, na.rm=TRUE), .groups="drop");
  nx <- nx %>% left_join(maxCap, by="NIDID") %>% mutate(elev_Ft = ifelse(elev_Ft > maxCap, NA, elev_Ft), storage_AF = ifelse(storage_AF > maxStor, NA, storage_AF), fstorage_AF = ifelse(fstorage_AF > maxfStor, NA, fstorage_AF)) %>% 
      select(-maxCap, -maxStor, -maxfStor)
  
  #old data
  fx <- old.data
  dateFormat = fx$date[1]
  if(substr(dateFormat,5,5) == "-") {dateFormatFinal = "%Y-%m-%d"}
  if(substr(dateFormat,5,5) != "-") {dateFormatFinal = "%m/%d/%Y"}
  fx$date <- as.Date(as.character(fx$date), dateFormatFinal) 
  fx$Year <- year(fx$date)
  fx$day_month <- substr(fx$date, 6, 10)
  fx$julian <-  as.integer(format(fx$date, "%j"))
  
  
  #what is the most recent date?
  old.last.date <- fx %>% group_by(NIDID) %>% filter(date == max(date, na.rm=TRUE)) %>% select(NIDID, date) %>% distinct() %>% rename(lastDate = date)
  
  #remove anything new before that date
  nx2 <- nx %>% left_join(old.last.date, by="NIDID") %>% filter(date > lastDate)
  fx.2014 <- fx %>% filter(Year>=2016) %>% select(NIDID, day_month, OT_FT, OT_ACFT) %>% distinct(); #2020 has complete data
  nx2 <- merge(nx, fx.2014, by.x=c("NIDID","day_month"), by.y=c("NIDID","day_month"), all.x=TRUE)  
  nx2 <- nx2 %>% mutate(percentStorage = round(storage_AF/OT_ACFT*100,2))
    nx2 <- nx2 %>% select(NIDID, name, date, Year, day_month, julian, elev_Ft, storage_AF, OT_FT, OT_ACFT, percentStorage); colnames(nx2) <- colnames(fx)
  
  #combine
  fx <- rbind(fx, nx2)
  
  #make sure no duplicates
  fx <- fx %>% distinct()
  #arrange by NIDID and date
  fx <- fx %>% arrange(NIDID, date)
  
  #SCOTT KERR HAS HAD A NEW SEDIMENT SURVEY
  scott.ot = 36639
  fx <- fx %>% mutate(OT_ACFT = ifelse(NIDID == "NC00300" & Year >=2017, 36639, OT_ACFT))
  
  fx <- fx %>% mutate(percentStorage = round(Storage_ACFT/OT_ACFT*100,2)) %>% mutate(Storage_ACFT = ifelse(percentStorage > 300, NA, Storage_ACFT), percentStorage = ifelse(percentStorage > 300, NA, percentStorage))
  summary(fx)
  write.csv(fx, paste0(swd_html, "reservoirs\\usace_dams.csv"), row.names=FALSE)

  

########################################################################################################################################################################################################################
#
#          UPDATE RESERVOIR STATUS AND STATS
#
########################################################################################################################################################################################################################
unique.sites <- unique(project.df$NIDID)

#set up data frame for stats and include year
stats <- as.data.frame(matrix(nrow=0,ncol=13));        colnames(stats) <- c("nidid", "julian", "min", "flow10", "flow25", "flow50", "flow75", "flow90", "max", "Nobs","startYr","endYr","date"); 
year.flow  <- as.data.frame(matrix(nrow=0, ncol=10));   colnames(year.flow) <- c("nidid", "name", "date", "year", "julian", "elev_ft","storage_af", "target_ft", "target_af", "percent_storage")

#Loop through each site and calculate statistics
  for (i in 1:length(unique.sites)){
    zt <- fx %>% filter(NIDID == unique.sites[i]) %>% filter(Year >= year(start.date))
    #summarize annual
    zt.stats <- zt %>% group_by(NIDID, julian) %>% summarize(Nobs = n(), min=round(min(percentStorage, na.rm=TRUE),4), flow10 = round(quantile(percentStorage, 0.10, na.rm=TRUE),4), flow25 = round(quantile(percentStorage, 0.25, na.rm=TRUE),4),
                                                      flow50 = round(quantile(percentStorage, 0.5, na.rm=TRUE),4), flow75 = round(quantile(percentStorage, 0.75, na.rm=TRUE),4), flow90 = round(quantile(percentStorage, 0.90, na.rm=TRUE),4), 
                                                      max = round(max(percentStorage, na.rm=TRUE),4), .groups="drop")
    zt.stats <- zt.stats %>% mutate(nidid = as.character(unique.sites[i]), startYr = min(zt$Year), endYr = max(zt$Year)) %>% select(nidid, julian, min, flow10, flow25, flow50, flow75, flow90, max, Nobs, startYr, endYr)
    if(dim(zt.stats)[1] == 366) {zt.stats$date = julian$month.day366}
    if(dim(zt.stats)[1] == 365) {zt.stats$date = julian$month.day365[c(1:365)]} 
    
    
    #fill dataframe
    stats <- rbind(stats, zt.stats)
    zt <- zt %>% filter(Year>=2017) %>% select(NIDID, Name, date, Year, julian, Elev_Ft, Storage_ACFT, OT_FT, OT_ACFT, percentStorage);    
      colnames(zt) <- c("nidid", "name", "date", "year", "julian", "elev_ft","storage_af", "target_ft", "target_af", "percent_storage")
    year.flow <- rbind(year.flow, zt)
    
    print(i)
  }
  bk.up <- stats
  summary(stats)
  summary(year.flow)
  
  stats <- stats %>% mutate(date2 = as.Date(paste0(end.year, "-",date), "%Y-%b-%d")) %>% mutate(month = substr(date,0,3))
  
  
  #Now attach most recent value to stream stats for the map
  recent.flow <- year.flow %>% group_by(nidid) %>% filter(is.na(storage_af) == FALSE) %>% filter(date == max(date)); #do we want to do most recent date or most recent date with data?
  current.stat <- merge(recent.flow[,c("nidid", "julian", "date", "percent_storage")], stats, by.x=c("nidid","julian"), by.y=c("nidid", "julian"), all.x=TRUE) %>% select(-date.y, -date2) %>% rename(date = date.x)
  
  #if else for this year and last years flow
  current.stat <- current.stat %>% mutate(status = ifelse(percent_storage <= flow10, "Extremely Dry", ifelse(percent_storage > flow10 & percent_storage <= flow25, "Very Dry", ifelse(percent_storage >= flow25 & percent_storage < flow50, "Moderately Dry", 
                                                  ifelse(percent_storage >= flow50 & percent_storage < flow75, "Moderately Wet", ifelse(percent_storage >= flow75 & percent_storage < flow90, "Very Wet", ifelse(percent_storage >= flow90, "Extremely Wet", "Unknown")))))))
  current.stat$status <- ifelse(is.na(current.stat$status), "unknown", current.stat$status)
  table(current.stat$status)
  
  #merge to sites geojson
  res.loc <- project.df %>% select(NIDID, Loc_ID, District, Name, url_link, geometry)
  nc.sites2 <- merge(res.loc, current.stat[,c("nidid","status","percent_storage","julian","flow50")], by.x="NIDID", by.y="nidid")
  geojson_write(nc.sites2, file=paste0(swd_html, "reservoirs\\usace_sites.geojson"))
  

  #rename nidid to site so can use same code as streamflow - used to make charts
  current.year <- year.flow %>% filter(year == year(max(date)));     last.year <- year.flow %>% filter(year == (year(max(date))-1));     
  stats.flow <- merge(stats, current.year[,c("nidid","julian","percent_storage")], by.x=c("nidid","julian"), by.y=c("nidid","julian"), all.x=TRUE) %>% rename(flow = percent_storage)
  stats.past <- merge(stats, last.year[,c("nidid", "julian", "percent_storage")], by.x=c("nidid", "julian"), by.y=c("nidid", "julian"), all.x=TRUE) %>% mutate(date2 = as.Date(paste0((end.year-1),"-",date), "%Y-%b-%d"))  %>% 
    rename(flow = percent_storage) %>% as.data.frame()
  stats.flow <- rbind(stats.past, stats.flow)
  
  stats.flow <- stats.flow %>% mutate(status = ifelse(flow <= flow10, "Extremely Dry", ifelse(flow > flow10 & flow <= flow25, "Very Dry", ifelse(flow >= flow25 & flow < flow50, "Moderately Dry", 
                                               ifelse(flow >= flow50 & flow < flow75, "Moderately Wet", ifelse(flow >= flow75 & flow < flow90, "Very Wet", ifelse(flow >= flow90, "Extremely Wet", "Unknown")))))))
  stats.flow <- stats.flow %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", ifelse(status=="Very Dry", "red", ifelse(status=="Moderately Dry", "orange", ifelse(status=="Moderately Wet", "cornflowerblue",
                                              ifelse(status=="Very Wet", "blue", ifelse(status=="Extremely Wet", "navy", "gray")))))))
  #attach flow of current year
  stats.flow <- stats.flow %>% rename(site = nidid)
  write.csv(stats.flow, paste0(swd_html, "reservoirs\\reservoir_stats.csv"), row.names=FALSE)
  
#remove files
  rm(stats, recent.flow, current.stat, stats.flow, stats.past, zt, fx, nx, lake_stor, lake_data, res.loc, nc.sites2, current.year, bk.up, year.flow, old.last.date)



