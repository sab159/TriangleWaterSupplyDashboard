#######################################################################################################################################################
#
# Updates streamflow data based on historic data collected
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# FEBRUARY 2021
# Update daily if desire
#
########################################################################################################################################################


#CALCULATE ROLLING AVERAGES MOVING FORWARD
#https://waterdata.usgs.gov/blog/moving-averages/
#owasa <- read_sf("https://geoconnex.us/ref/pws/NC0368010")
#################################################################################################################################
#       SET UP INFORMATION TO CALL API
#################################################################################################################################
#Identify parameter of interest: https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&inline=true&group_cd=%
pcode = '00060' #discharge (cfs)
#Identify statistic code for daily values: https://help.waterdata.usgs.gov/code/stat_cd_nm_query?stat_nm_cd=%25&fmt=html
scode = "00003"  #mean
#pick service
serv <- "dv"


#################################################################################################################################
#
##############                               LOAD DATA
#
#################################################################################################################################
#What sites are available in NC?
nc.sites <- read_sf(paste0(swd_html, "streamflow\\stream_gauge_sites.geojson")) %>% select(site, name, huc8, startYr, endYr, nYears, geometry)
ws.bounds <- read_sf(paste0(swd_html, "water_supply_watersheds.geojson")) %>% select(-nSheds, -drawFile)
all.data <- read.csv(paste0(swd_html, "streamflow\\all_stream_data.csv"), colClasses=c("site" = "character")) %>% mutate(date = as.Date(date, format="%Y-%m-%d"))


#################################################################################################################################
#
#                 IDENTIFY GAUGES WITH PWSID WATER SUPPLY WATERSHEDS
#
#################################################################################################################################
#intersect together gauge location with watershed bounds
gauges_huc <- st_intersection(nc.sites, ws.bounds);
  
#merge names together
zt <- gauges_huc %>% as.data.frame() %>% select(site, STREAM_NAM) %>% distinct()
nc.sites <- merge(nc.sites, zt, by.x="site", by.y="site", all.x=TRUE)
table(nc.sites$STREAM_NAM, useNA="ifany")

#go through stream sites and calculate statistics
unique.sites <- unique(nc.sites$site)


#set up data frame for stats and include year
year.flow  <- as.data.frame(matrix(nrow=0, ncol=4));    colnames(year.flow) <- c("site", "date", "julian", "flow")
#Loop through each site and calculate statistics
for (i in 1:length(unique.sites)){
  old.data <- all.data %>% filter(site==unique.sites[i]) %>% filter(date==max(date))
  zt <- readNWISdv(siteNumbers = unique.sites[i], parameterCd = pcode, statCd = scode, startDate=(old.data$date[1]+1), endDate = end.date); #only read in new data
    zt <- renameNWISColumns(zt);
  
  if (dim(zt)[1] > 0)  {
    zt <- zt %>% mutate(julian = as.POSIXlt(Date, format = "%Y-%m-%d")$yday); #calculates julian date
    
    zt <- zt %>% dplyr::select(site_no, Date, julian, Flow);    colnames(zt) <- c("site", "date", "julian", "flow")
    zt <- zt %>% group_by(site, date, julian) %>% summarize(flow = median(flow, na.rm=TRUE), .groups="drop")
    
    year.flow <- rbind(year.flow, zt)
  }
    
 print(paste0(i, " is ", round(i/length(unique.sites)*100,2), "% done"))
}
summary(year.flow)

all.data <- rbind(all.data, year.flow) %>% arrange(site, date)
write.csv(all.data, paste0(swd_html, "streamflow\\all_stream_data.csv"), row.names=FALSE)
#do rolling average etc next

#bind all.data and year flow together and calculate 7 day rolling average (function is in global api)
#Check for missing days, if so, add NA rows: #https://waterdata.usgs.gov/blog/moving-averages/
year.flow  <- as.data.frame(matrix(nrow=0, ncol=4));    colnames(year.flow) <- c("site", "date", "julian", "flow")
stats <- as.data.frame(matrix(nrow=0,ncol=13));        colnames(stats) <- c("Site", "julian", "min", "flow10", "flow25", "flow50", "flow75", "flow90", "max", "Nobs","startYr","endYr","date"); 
for (i in 1:length(unique.sites)){
  zt <- all.data %>% filter(site==unique.sites[i])
  
  if(as.numeric(diff(range(zt$date))) != (nrow(zt)+1)){
    fullDates <- seq(from=min(zt$date), to = max(zt$date), by="1 day")
    fullDates <- data.frame(date = fullDates, stringsAsFactors = FALSE)
    zt <- full_join(zt, fullDates,by=c("date")) %>% arrange(date) %>% mutate(site=unique.sites[i], julian = as.POSIXlt(date, format = "%Y-%m-%d")$yday)
  }
  zt <- zt %>% mutate(rollMean=round(as.numeric(ma(flow)),4)) %>% mutate(year = year(date))
  
  #summarize annual
  zt.stats <- zt %>% group_by(site, julian) %>% summarize(Nobs = n(), min=round(min(rollMean, na.rm=TRUE),2), flow10 = round(quantile(rollMean, 0.10, na.rm=TRUE),2), flow25 = round(quantile(rollMean, 0.25, na.rm=TRUE),2),
                                                    flow50 = round(quantile(rollMean, 0.5, na.rm=TRUE),2), flow75 = round(quantile(rollMean, 0.75, na.rm=TRUE),2), flow90 = round(quantile(rollMean, 0.90, na.rm=TRUE),2), 
                                                    max = round(max(rollMean, na.rm=TRUE),2),  .groups="drop")
  
  zt.stats <- zt.stats %>% mutate(startYr = min(zt$year), endYr = max(zt$year)) %>% select(site, julian, min, flow10, flow25, flow50, flow75, flow90, max, Nobs, startYr, endYr)
  if(dim(zt.stats)[1] == 366) {zt.stats$date = julian$month.day366; }
  if(dim(zt.stats)[1] == 365) {zt.stats$date = subset(julian, julian <= 364)$month.day365; }
  
  zt <- zt %>% filter(year>=(current.year-2)) %>% dplyr::select(site, date, julian, rollMean);    colnames(zt) <- c("site", "date", "julian", "flow")

#fill dataframe
  stats <- rbind(stats, zt.stats)
  year.flow <- rbind(year.flow, zt)
  print(paste0(i, " with ", round(i/length(unique.sites)*100,2), "% done"))
}
summary(stats)
summary(year.flow)

#for current stats - find out status of streamflow for sites and for flow points
#set up date
stats <- stats %>% mutate(date2 = as.Date(paste0(current.year,"-",date), "%Y-%b-%d")) %>% as.data.frame()

#Now attach most recent value to stream stats
recent.flow <- year.flow %>% group_by(site) %>% filter(is.na(flow)==FALSE) %>% filter(date == max(date))
current.stat <- merge(recent.flow[,c("site", "julian", "flow")], stats, by.x=c("site","julian"), by.y=c("site", "julian"))
current.stat <- current.stat %>% mutate(flow = round(flow, 2))

#if else
current.stat <- current.stat %>% mutate(status = ifelse(flow <= flow10, "Extremely Dry", ifelse(flow > flow10 & flow <= flow25, "Very Dry", ifelse(flow >= flow25 & flow < flow50, "Moderately Dry", 
                                                 ifelse(flow >= flow50 & flow < flow75, "Moderately Wet", ifelse(flow >= flow75 & flow < flow90, "Very Wet", ifelse(flow >= flow90, "Extremely Wet", "Unknown")))))))
current.stat$status <- ifelse(is.na(current.stat$status), "unknown", current.stat$status)
table(current.stat$status, useNA="ifany")
#set those that are not collecting data to unknown
current.stat <- current.stat %>% mutate(status = ifelse(endYr < current.year & julian > 30, "unknown", ifelse(endYr < (current.year-1), "unknown", status)))
table(current.stat$status)

#merge to geojson file with current status for map display
nc.sites2 <- merge(nc.sites, current.stat[,c("site","status","flow","julian","date","flow50")], by.x="site", by.y="site")
geojson_write(nc.sites2, file=paste0(swd_html, "streamflow\\stream_gauge_sites.geojson"))


#to create stats diagram with past and current year - need to make separate for dates and then rbind
year.flow2 <- year.flow %>% mutate(flow = round(flow, 2)) %>% filter(date >= as.Date(paste0(current.year, "-01-01"), "%Y-%m-%d"))
year.past <- year.flow %>% mutate(flow = round(flow, 2)) %>% filter(date >= as.Date(paste0((current.year-1),"-01-01"), "%Y-%m-%d") & date <= as.Date(paste0((current.year-1),"-12-31"), "%Y-%m-%d"))

stats2 <- merge(stats, year.flow2[,c("site", "julian", "flow")], by.x=c("site", "julian"), by.y=c("site", "julian"), all.x=TRUE)
stats.past <- merge(stats, year.past[,c("site", "julian", "flow")], by.x=c("site", "julian"), by.y=c("site", "julian"), all.x=TRUE) %>% mutate(date2 = as.Date(paste0((current.year-1),"-",date), "%Y-%b-%d")) %>% as.data.frame()
stats2 <- rbind(stats.past, stats2)

#stats2 <- stats2 %>% mutate(perFlow = round(flow/flow50*100,2)); summary(stats2)
stats2 <- stats2 %>% mutate(status = ifelse(flow <= flow10, "Extremely Dry", ifelse(flow > flow10 & flow <= flow25, "Very Dry", ifelse(flow >= flow25 & flow < flow50, "Moderately Dry", 
                                            ifelse(flow >= flow50 & flow < flow75, "Moderately Wet", ifelse(flow >= flow75 & flow < flow90, "Very Wet", ifelse(flow >= flow90, "Extremely Wet", "Unknown")))))))
#stats2 <- stats2 %>% mutate(month = substr(date,0,3))
stats2 <- stats2 %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", ifelse(status=="Very Dry", "red", ifelse(status=="Moderately Dry", "orange", ifelse(status=="Moderately Wet", "cornflowerblue",
                                          ifelse(status=="Very Wet", "blue", ifelse(status=="Extremely Wet", "navy", "gray"))))))) %>% mutate(colorStatus = ifelse(is.na(colorStatus), "gray", colorStatus))
stats2 <- stats2 %>% arrange(site, date2)
table(stats2$status, useNA="ifany")
table(stats2$colorStatus, useNA="ifany")

#write out the csv to draw daily flow based on stream status
write.csv(stats2, paste0(swd_html, "streamflow\\stream_stats.csv"), row.names=FALSE)


#write out the current stats for water supply watershed
current.stat2 <- current.stat %>% select(site, julian, date, status) %>% mutate(month = substr(date,0,3))
site.huc <- cbind(nc.sites2$site, nc.sites2$huc8, nc.sites$STREAM_NAM) %>% as.data.frame(); colnames(site.huc) <- c("site", "huc8", "ws_watershed")
current.stat2 <- merge(current.stat2, site.huc, by.x="site", by.y="site", all.x=TRUE)
write.csv(current.stat2, paste0(swd_html, "streamflow\\sites_status.csv"), row.names=FALSE)


rm(site.huc, current.stat2, stats2, nc.sites2, year.flow2, year.flow, year.past, stats.past, recent.flow, current.stat, zt, stats, nc.sites, gages.huc)



