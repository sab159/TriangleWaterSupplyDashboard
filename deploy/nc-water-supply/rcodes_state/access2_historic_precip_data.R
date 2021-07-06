#######################################################################################################################################################
#
# Creates initial pull of precipitation data from the NC State Climate Office Cloud. Each API is given a limited number of pulls each month. This 
# maxed out pulls. Do not repeat.
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# FEBRUARY 2021
#
########################################################################################################################################################

###################################################################################################################################
#
#          READ IN LIST OF STATIONS - MANUALLY CREATED HERE: https://api.climate.ncsu.edu/locations
#          Because there is an api limit, it is very important to call the minimum amount
#
###################################################################################################################################
#read in precipitation location list
pcp.loc <- read_excel(paste0(swd_html, "pcp\\ncsu_api_locations.xlsx"), sheet="all") %>% mutate(startYear = year(start_date), endYear = year(end_date), nYears = endYear-startYear)
pcp.loc <- pcp.loc %>% mutate(id = ifelse(id=="2098197", "02098197", id))

#assumes empty character if api not called yet
pcp.list <- unique(subset(pcp.loc, is.na(api))$id)

    
###################################################################################################################################
#
#          PULL TO BUILD DATABASE
#          
####################################################################################################################################
#create databases
nc.loc <- as.data.frame(matrix(nrow=0, ncol=12)); colnames(nc.loc) <- c("locID","network","name","city","county","state","lat_y","long_x","elev_ft","agency","startDate","endDate")
nc.data <-as.data.frame(matrix(nrow=0, ncol=12)); colnames(nc.data) <- c("locID","date","var","value","unit","score","nettype","vartype","obtime","obtype","obnum","value_accum")

#loop through locaiton ids to build database. Go from 1990 to present
#count how much of api data using
total.obsv = 0;
url_base = "https://api.climate.ncsu.edu/data.php?var=precip&obtype=D&output=csv&loc=location="; #for daily precipitation
#for (i in 1:length(pcp.list)){
for (i in 81:88){
  #make empty
  zt = NA
  yt.df = NA
  yt.loc = NA
  #build api link
  url_loc = pcp.list[i]

  #need to readjust time if starts later
  zt.pcp <- pcp.loc %>% filter(id == pcp.list[i])
  if(zt.pcp$startYear >= 1990) {
    url_time_start = paste0("&start=",substr(zt.pcp$start_date,0,10))
  }
  if(zt.pcp$startYear < 1990) {
    url_time_start = paste0("&start=", start.date)
  }
  url_time_end = paste0("&end=,",today)
  url_hash = paste0("&hash=", ncsco.key)
  
  full_url = paste0(url_base, url_loc, url_time_start, url_time_end, url_hash)  
  
  #call url
  zt <- readLines(full_url, warn=FALSE)
  #start cleaning
 
  yt.obsv <- as.numeric(str_split(zt[11],": ", simplify=TRUE)[1,2])
  #call location data
  for (j in 60:72){
    #yt.loc <- str_split(zt[62],",", simplify = TRUE)[1,(2:13)] %>% t() %>% as.data.frame()
    foo <- str_split(zt[j], ",", simplify=TRUE)
    if(foo[1,1] == "## Location ID"){
      yt.loc <- str_split(zt[j+1],",", simplify = TRUE)[1,(2:13)] %>% t() %>% as.data.frame()
      break
    }
    print(j)
  }
  colnames(yt.loc) <- c("locID","network","name","city","county","state","lat_y","long_x","elev_ft","agency","startDate","endDate")
  
  #call value data
  yt.df <- read_csv(zt, comment="##")
  
  #make sure datetime and obtime are in standardized format
  yt.df <- yt.df %>% mutate(datetime = as.Date(datetime, format="%Y-%m-%d"), obtime = as.Date(obtime, format="%Y-%m-%d"))
  
  #total obs
  total.obsv = total.obsv + yt.obsv;
  nc.loc <- rbind(nc.loc, yt.loc);
  nc.data <- rbind(nc.data, yt.df);
  
  print(paste0(i, ": ", total.obsv, " which is ", round(total.obsv/600000*100,2),"% of monthly quota"))
}
bk.up.loc <- nc.loc
bk.up.data <- nc.data

#remove current day... is missing value
nc.data <- nc.data %>% group_by(location) %>% filter(datetime < max(datetime)) %>% distinct()

#save files
write.csv(nc.data, paste0(swd_html, "pcp\\boneyard\\ncsu_data_restofState2.csv"), row.names = FALSE)
write.csv(nc.loc, paste0(swd_html, "pcp\\boneyard\\ncsu_locations_restofState2.csv"), row.names = FALSE)











#mutate and save out slimmed version
nc.data %>% filter(score==3) %>% as.data.frame()# keep?
table(nc.data$unit); #should all be inches... MV seems to be missing value
table(nc.data$nettype, useNA = "ifany"); #should all be measured
table(nc.data$vartype, useNA = "ifany"); #these are all A - aggregate of multiple variables?
table(nc.data$obtype, useNA="ifany"); #these should all be d for daily

#rename columns and minimize
pcp.data <- nc.data %>% select(location, datetime, value, value_accum) %>% mutate(value = as.numeric(value))
colnames(pcp.data) <- c("id", "date", "pcp_in", "cum_pcp")
#set NA / bad data to zero
#pcp.data[is.na(pcp.data)] <-0

#convert date time to just date and add year column
pcp.data <- pcp.data %>% mutate(date = as.POSIXct(date, format="%Y-%m-%d"), year = year(date), month = month(date), day = day(date))
pcp.data <- pcp.data %>% arrange(id, date) %>% distinct()
table(pcp.data$id, pcp.data$year)


check.last.date <- pcp.data %>% group_by(id) %>% filter(is.na(pcp_in) == FALSE) %>% filter(date == max(date)) %>% dplyr::select(id, date, month)
table(substr(check.last.date$date,0,10))


#merge with triangle data --> one time
nc.data.triangle<-read.csv(paste0(swd_html, "pcp\\pcp_data.csv")) %>% mutate(date = as.POSIXct(date, format="%Y-%m-%d"), day = day(date))
nc.loc.triangle <- read.csv(paste0(swd_html, "pcp\\pcp_locations.csv"))

nc.loc2 <- rbind(nc.loc.triangle, nc.loc)
nc.data2 <- rbind(nc.data.triangle, pcp.data)# %>% mutate(date = paste0(year,"-",month,"-",day))

write.csv(nc.data2, paste0(swd_html, "pcp\\pcp_data.csv"), row.names = FALSE)
write.csv(nc.loc2, paste0(swd_html, "pcp\\pcp_locations.csv"), row.names = FALSE)

#convert station sites into an sf file
sites <- st_as_sf(nc.loc2, coords = c("long_x", "lat_y"), crs = 4326); 
mapview::mapview(sites)



###################################################################################################################################
#
#          DROUGHT DATABASE
#          
####################################################################################################################################
#download tables for HUCS of interest 
huc8 <- read_sf(paste0(swd_html, "huc8.geojson"))
huc.list <- huc8$huc8

end.date <- paste0("12/31/", year(today))

#create dataframe and pull new data
drought.time <- as.data.frame(matrix(nrow=0, ncol=9)); colnames(drought.time) <- c("huc8","name","date","none","d0","d1","d2","d3","d4")
for (m in 1:length(huc.list)){
  full_url <-paste0("https://usdmdataservices.unl.edu/api/HUCStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=",huc.list[m],"&startdate=01/01/2000&enddate=", end.date, "&statisticsType=1")
  api.data <- GET(full_url, timeout(15000)) #use httr libirary to avoid timeout
  df <- content(api.data, "parse")
  
  for (i in 1:length(df)){
    zt.name <- as.character(subset(huc8, huc8==huc.list[m])$name) 
    zt = tibble(
      huc8 = as.character(huc.list[m]),
      name = zt.name,
      date = df[[i]]$ValidStart,
      none = df[[i]]$None,
      d0 = df[[i]]$D0,
      d1 = df[[i]]$D1,
      d2 = df[[i]]$D2,
      d3 = df[[i]]$D3,
      d4 = df[[i]]$D4
    )
    drought.time <- rbind(drought.time, zt)
  }
  print(zt.name)
}
table(drought.time$huc8)

#TAKES 25 SECONDS TO RUN FOR FULL YEAR... IN FUTURE WILL NEED TO SHORTEN
drought2 <- drought.time %>% mutate(date = as.Date(date, "%Y-%m-%d"), none = as.numeric(none), d0 = as.numeric(d0), d1 = as.numeric(d1), d2 = as.numeric(d2), d3=as.numeric(d3), d4=as.numeric(d4)) %>% arrange(huc8, date)
#it seems that drought is cumulative
drought2 <- drought2 %>% mutate(d4x = d4, d3x = d3 - d4, d2x = d2-d3, d1x = d1-d2, d0x = d0-d1)
#slim and save file
drought2 <- drought2 %>% select(huc8, name, date, none, d0x, d1x, d2x, d3x, d4x)
drought2 <- drought2 %>% arrange(huc8, date) %>% distinct()
write.csv(drought2, paste0(swd_html, "drought\\percentAreaHUC.csv"), row.names=FALSE)

