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
pcp.loc <- read_excel(paste0(swd_html, "pcp\\ncsu_api_locations.xlsx"), sheet="Sheet1") %>% mutate(startYear = year(start_date), endYear = year(end_date), nYears = endYear-startYear)
pcp.loc <- pcp.loc %>% mutate(id = ifelse(id=="2098197", "02098197", id))

pcp.list <- unique(subset(nc.loc, api == "yes")$id)
today = substr(Sys.time(),1,10); today;

    
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
for (i in 1:length(pcp.list)){
  #build api link
  url_loc = pcp.list[i]
  url_base = "https://api.climate.ncsu.edu/data.php?var=precip&obtype=D&output=csv&loc=location="; #for daily precipitation
  
  #need to readjust time if starts later
  zt.pcp <- pcp.loc %>% filter(id == pcp.list[i])
  if(zt.pcp$startYear >= 1990) {
    url_time_start = paste0("&start=",substr(zt.pcp$start_date,0,10))
  }
  if(zt.pcp$startYear < 1990) {
    url_time_start = paste0("&start=1990-01-01")
  }
  url_time_end = paste0("&end=,",today)
  url_hash = paste0("&hash=", ncsco.key)
  
  full_url = paste0(url_base, url_loc, url_time_start, url_time_end, url_hash)  
  
  #call url
  zt <- readLines(full_url, warn=FALSE)
  #start cleaning
  yt.obsv <- as.numeric(str_split(zt[11],": ", simplify=TRUE)[1,2])
  
  #call location data
  yt.loc <- str_split(zt[62],",", simplify = TRUE)[1,(2:13)] %>% t() %>% as.data.frame()
  colnames(yt.loc) <- c("locID","network","name","city","county","state","lat_y","long_x","elev_ft","agency","startDate","endDate")
  
  #call value data
  yt.df <- read_csv(zt, comment="##")
  
  #total obs
  total.obsv = total.obsv + yt.obsv;
  nc.loc <- rbind(nc.loc, yt.loc);
  nc.data <- rbind(nc.data, yt.df);
  
  print(paste0(i, ": ", total.obsv))
}
#save files

write.csv(nc.data, paste0(swd_html, "pcp\\ncsu_triangle_data.csv"), row.names = FALSE)
write.csv(nc.loc, paste0(swd_html, "pcp\\ncsu_triangle_locations.csv"), row.names = FALSE)
#convert station sites into an sf file
sites <- st_as_sf(nc.loc, coords = c("long_x", "lat_y"), crs = 4326); 
mapview::mapview(sites)



