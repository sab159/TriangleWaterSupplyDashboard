#######################################################################################################################################################
#
# Creates initial map layers and downloads historic data for the dashboard
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# FEBRUARY 2021
# Run after acceess1_static_map_layers.rcode
#
########################################################################################################################################################



######################################################################################################################################################################
#
#   USGS STREAM GAUGE DATA - CREATE MAP LAYER AND PULL HISTORIC DATA
#
######################################################################################################################################################################
# set up variables to retrieve USGS data - here mean daily data
pcode = '00060'; #discharge (cfs); #Identify parameter of interest: https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&inline=true&group_cd=%
scode = "00003";  #mean;  #Identify statistic code for daily values: https://help.waterdata.usgs.gov/code/stat_cd_nm_query?stat_nm_cd=%25&fmt=html
serv <- "dv"; #pick service... daily value

#Identify start and end dates... I went back to 1990 but can change to different time periods
start.date = "1990-01-01"
end.year = year(Sys.time())
end.date = paste0(end.year, "-12-31")

#read in file to constrain stream gauge search
link.df <- read.csv(paste0(swd_html, "link_pwsid_watershed.csv"), colClasses=c("huc8" = "character")) %>% mutate(huc8 = ifelse(nchar(huc8) ==7, paste0("0",huc8), huc8)); #csv docks first 0... 


#################################################################################################################################
#What sites have data available and are currently collecting data?
nc.sites <- readNWISdata(stateCd="NC", parameterCd = pcode, service = "site", seriesCatalogOutput=TRUE);
  length(unique(nc.sites$site_no))
  head(nc.sites)

#convert dates from character to date
nc.sites <- nc.sites %>% mutate(begin_year = as.numeric(substr(begin_date,1,4)), end_year = as.numeric(substr(end_date,1,4)))
#filter data to only keep if currently collecting data and has at least five years of data and located within the list of HUC8's serving utilities (could substring to make it huc6 if perferred)
nc.sites2 <- filter(nc.sites, parm_cd %in% pcode) %>% filter(stat_cd %in% scode) %>% filter(end_year == end.year) %>%  mutate(period = end_year-begin_year) %>% filter(period >= 5) %>% filter(huc_cd %in% link.df$huc8)
  length(unique(nc.sites2$site_no))
nc.sites2 <- nc.sites2 %>% select(site_no, station_nm, dec_lat_va, dec_long_va, huc_cd, begin_year, end_year, period)
  colnames(nc.sites2) <- c("site", "name", "latitude", "longitude", "huc8", "startYr", "endYr", "nYears")


#calculate unique sites
unique.sites <- unique(nc.sites2$site)
#for graphs - how far back do you want to see details? Right now set for last 2 years
yearFlowStart = end.year - 2;

#set up data frame for stats and include year
stats <- as.data.frame(matrix(nrow=0,ncol=12));        colnames(stats) <- c("Site", "julian", "min", "flow10", "flow25", "flow50", "flow75", "flow90", "max", "Nobs","startYr","endYr"); 
year.flow  <- as.data.frame(matrix(nrow=0, ncol=4));    colnames(year.flow) <- c("site", "date", "julian", "flow_cms")

#Loop through each site, pulls data, and calculate statistics
for (i in 1:length(unique.sites)){
  zt <- readNWISdv(siteNumbers = unique.sites[i], parameterCd = pcode, statCd = scode, startDate=start.date, endDate = end.date)
    zt <- renameNWISColumns(zt);
  zt <- zt %>% mutate(julian = as.POSIXlt(Date, format = "%Y-%m-%d")$yday); #calculates julian date
  zt <- zt %>% mutate(Flow_cms = Flow*0.028316847, Year = year(Date))
  
  #summarize annual
  zt.stats <- zt %>% group_by(julian) %>% summarize(Nobs = n(), min=min(Flow_cms, na.rm=TRUE), flow10 = quantile(Flow_cms, 0.10, na.rm=TRUE), flow25 = quantile(Flow_cms, 0.25, na.rm=TRUE),
                                                    flow50 = quantile(Flow_cms, 0.5, na.rm=TRUE), flow75 = quantile(Flow_cms, 0.75, na.rm=TRUE), flow90 = quantile(Flow_cms, 0.90, na.rm=TRUE), max = max(Flow_cms, na.rm=TRUE),
                                                    .groups="keep")
  zt.stats <- zt.stats %>% mutate(site = as.character(unique.sites[i]), startYr = min(zt$Year), endYr = max(zt$Year)) %>% select(site, julian, min, flow10, flow25, flow50, flow75, flow90, max, Nobs, startYr, endYr)
    
  #fill dataframe
  stats <- rbind(stats, zt.stats)
  
  zt <- zt %>% filter(Year >= yearFlowStart) %>% select(site_no, Date, julian, Flow_cms);    colnames(zt) <- c("site", "date", "julian", "flow")
  year.flow <- rbind(year.flow, zt)
    
    print(i)
}
summary(stats)
summary(year.flow)

#write files
write.csv(stats, paste0(swd_html, "streamflow\\stream_stats.csv"), row.names=FALSE)

#create geojson 
nc_points <- st_as_sf(nc.sites2, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
mapview::mapview(nc_points)
geojson_write(nc_points, file =  paste0(swd_html, "streamflow\\stream_gauge_sites.geojson"))
############################################################################################################################



  
  
  
  