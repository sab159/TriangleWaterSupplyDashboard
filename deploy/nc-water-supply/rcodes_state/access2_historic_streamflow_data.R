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



#################################################################################################################################
#What sites have data available and are currently collecting data?
nc.sites <- readNWISdata(stateCd="NC", parameterCd = pcode, service = "site", seriesCatalogOutput=TRUE);
  length(unique(nc.sites$site_no))
  head(nc.sites)

#convert dates from character to date
nc.sites <- nc.sites %>% mutate(begin_year = as.numeric(substr(begin_date,1,4)), end_year = as.numeric(substr(end_date,1,4)))
#filter data to only keep if currently collecting data and has at least five years of data and located within the list of HUC8's serving utilities (could substring to make it huc6 if perferred)
nc.sites2 <- filter(nc.sites, parm_cd %in% pcode) %>% filter(stat_cd %in% scode) %>% filter(end_year == end.year) %>%  mutate(period = end_year-begin_year) %>% filter(period >= 5) 
  length(unique(nc.sites2$site_no))
nc.sites2 <- nc.sites2 %>% dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va, huc_cd, begin_year, end_year, period)
  colnames(nc.sites2) <- c("site", "name", "latitude", "longitude", "huc8", "startYr", "endYr", "nYears")


#calculate unique sites
unique.sites <- unique(nc.sites2$site)

#for graphs - how far back do you want to see details? Right now set for last 2 years
yearFlowStart = end.year - 2;

#set up data frame for stats and include year
year.flow  <- as.data.frame(matrix(nrow=0, ncol=5));    colnames(year.flow) <- c("site", "date", "julian", "flow_cfs")
#Loop through each site, pulls data, and calculate statistics
for (i in 1:length(unique.sites)){
  zt <- readNWISdv(siteNumbers = unique.sites[i], parameterCd = pcode, statCd = scode, startDate=start.date, endDate = end.date)
    zt <- renameNWISColumns(zt);
  zt <- zt %>% mutate(julian = as.POSIXlt(Date, format = "%Y-%m-%d")$yday); #calculates julian date
  
  zt <- zt %>% dplyr::select(site_no, Date, julian, Flow);    colnames(zt) <- c("site", "date", "julian", "flow_cfs")
  zt <- zt %>% group_by(site, date, julian) %>% summarize(flow = median(flow, na.rm=TRUE), .groups="drop")
  
  year.flow <- rbind(year.flow, zt)
    
  print(paste0(i, " with ", round(i/length(unique.sites)*100,2), "% done"))
}
summary(year.flow)

#write files
write.csv(year.flow, paste0(swd_html, "streamflow\\all_stream_data.csv"), row.names=FALSE)

#create geojson 
nc_points <- st_as_sf(nc.sites2, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
mapview::mapview(nc_points)
geojson_write(nc_points, file =  paste0(swd_html, "streamflow\\stream_gauge_sites.geojson"))
############################################################################################################################



  
  
  
  