###################################################################################################################################################
#
# Creates initial map layers and downloads historic data for the dashboard
# CREATED BY LAUREN PATTERSON & KYLE ONDA @ THE INTERNET OF WATER
# FEBRUARY 2021
# Run anytime... change county.list if desire. 
#
###################################################################################################################################################


######################################################################################################################################################################
#
#   READ IN GROUNDWATER SITES AND GROUNDWATER DATA
#
######################################################################################################################################################################
#set up base url
#https://cida.usgs.gov/ngwmn_cache/sos?request=GetCapabilities&service=SOS&acceptVersions=2.0.0&acceptedFormats=text/xml
#because of set up did a manual download of sites in NC
nc.sites <- read.csv(paste0(swd_html, "gw\\gw_sites.csv")); #all NAD83
all.data <- read.csv(paste0(swd_html, "gw\\all_gw_levels.csv")) %>% mutate(date = as.Date(date, format="%Y-%m-%d"))

######################################################################################################################################################################
#
# PULL IN GW LEVEL DATA DYNAMICALLY
#
#####################################################################################################################################################################
#break up by agency to dynamically pull datea
usgs.sites <- nc.sites %>% filter(AgencyCd=="USGS")
dwr.sites <- nc.sites %>% filter(AgencyCd=="NCDWR")

############################################     RUN FOR USGS   #####################################################################################################
#calculate unique sites
unique.usgs.sites <- unique(usgs.sites$site)

#set up data frame for stats and include year
stats <- as.data.frame(matrix(nrow=0,ncol=13));        colnames(stats) <- c("site", "julian", "min", "flow10", "flow25", "flow50", "flow75", "flow90", "max", "Nobs","startYr","endYr","date"); 
year.flow  <- as.data.frame(matrix(nrow=0, ncol=4));    colnames(year.flow) <- c("site", "date", "julian", "flow_cms")

#Loop through each site and calculate statistics
for (i in 1:length(unique.usgs.sites)){
  old.data <- all.data %>% filter(site == unique.usgs.sites[i]) %>% filter(date == max(date))
    start.date.data <- max(old.data$date)+1
  url = paste0("https://waterdata.usgs.gov/nc/nwis/dv?cb_72019=on&format=rdb&site_no=", unique.usgs.sites[i],"&referred_module=sw&period=&begin_date=",start.date.data,"&end_date=",end.date)
  #call url
  zt <- read_csv(url, comment="#")
  colnames(zt)<-"df_split";
  zt <- zt[-1,] #remove first row
  
  if (dim(zt)[1] > 0){
    #start cleaning
    zt <- data.frame(do.call('rbind', strsplit(as.character(zt$df_split),'\t',fixed=TRUE)))
    print(paste(unique.usgs.sites[i], "has ", dim(zt)[2], " columns"))
    
    if(dim(zt)[2]==5){
      zt <- zt %>% rename(site = X2, date=X3, depth_below_surface_ft = X4) %>% select(site, date, depth_below_surface_ft) %>% mutate(depth_below_surface_ft = as.numeric(as.character(depth_below_surface_ft)))
    }
    if(dim(zt)[2]==9){
      zt <- zt %>% rename(site = X2, date=X3, depth_below_surface_ft = X8) %>% select(site, date, depth_below_surface_ft) %>% mutate(depth_below_surface_ft = as.numeric(as.character(depth_below_surface_ft)))
    }
    #if missing data the site number is repeated or non-numeric value
    zt <- zt %>% mutate(depth_below_surface_ft = ifelse(depth_below_surface_ft > 99999, NA, depth_below_surface_ft))
    
    zt <- zt %>% mutate(julian = as.POSIXlt(date, format = "%Y-%m-%d")$yday, year = year(date))# %>% mutate(date = paste0(month(Date, label=TRUE, abbr=TRUE),"-",day(Date))); #calculates julian date as.Date(c("2007-06-22", "2004-02-13"))
    #fill dataframe
    zt <- zt %>% select(site, date, julian, depth_below_surface_ft);    colnames(zt) <- c("site", "date", "julian", "depth_ft")
    year.flow <- rbind(year.flow, zt)
  } #end if new data
  
  print(i)
}
summary(year.flow)


############################################     RUN FOR NCDWR   #####################################################################################################
#url_base <- "https://www.ncwater.org/Data_and_Modeling/Ground_Water_Databases/potmaps/gwbdatafiles/"   #m53l1115239lev.txt"
#                 ONLY HAS DATA FOR LAST TWO YEARS
#
#########################################################################################################################################################################
#This link seems to change frequently... so unfortunately need to rerun each time (takes about 30 minutes for 600+ sites)...
url.sites <- dwr.sites %>% mutate(site2 = site) %>%  separate(site, into = c("text", "num", "text2"), sep = "(?<=[A-Za-z])(?=[0-9])(?<=[A-Za-z])") %>% mutate(url_site = paste0(text,"**",num,text2))
url.sites$link = NA

#This takes longer than 30 minutes to run
unique.dwr.sites <- unique(url.sites$url_site)
for (i in 1:length(unique.dwr.sites)){
  test <- xml2::read_html(paste0("https://www.ncwater.org/?page=536&id=",unique.dwr.sites[i]))
  a <- test %>%
    rvest::html_node("main") %>%
    rvest::html_node("div") %>%
    rvest::html_node("table") %>%
    rvest::html_nodes(xpath="//a") 
  a.test <- grep('elev.txt', a, value=TRUE)
  
  for (v in 1:length(a)){
    a.test = grep('lev.txt', html_attr(a[v], "href"), value=TRUE)
    if(length(a.test) > 0) {
      url.sites$link[i] <- paste0("https://www.ncwater.org",html_attr(a[v],"href")) 
    }
  }
  print(paste(i, "-", url.sites$link[i]))
}  
bk.up <- url.sites
#remove old nc.sites links
nc.sites <- nc.sites %>% dplyr::select(-link)
nc.sites <- merge(nc.sites, url.sites[,c("site2","link")], by.x="site", by.y="site2", all.x=TRUE)
#set usgs link - https://waterdata.usgs.gov/monitoring-location/355944079013401/#parameterCode=72019&period=P7D
nc.sites <- nc.sites %>% mutate(link = ifelse(AgencyCd=="USGS", paste0("https://waterdata.usgs.gov/monitoring-location/", site, "#parameterCode=72019&period=P7D"), link))
write.csv(nc.sites, paste0(swd_html, "gw\\gw_sites.csv"), row.names = FALSE)


#Build on USGS dataframe
#Loop through each site and calculate statistics
unique.dwr.sites <- unique(dwr.sites$site)
for (i in 1:length(unique.dwr.sites)){
  zt.site <- nc.sites %>% filter(site == unique.dwr.sites[i])
  old.data <- all.data %>% filter(site == unique.dwr.sites[i]) %>% filter(date == max(date))
  start.date.data <- max(old.data$date)+1
  
  zt <- read.csv(zt.site$link[1], header=FALSE, sep="\t")
  #print(zt[dim(zt)[1],])
    colnames(zt) <- c("date", "depth_below_surface_ft", "elevation")
    zt <- zt %>% mutate(julian = as.POSIXlt(date, format = "%Y-%m-%d")$yday, year = year(date), site = zt.site$site[1]) %>% filter(date >= start.date.data)
    #999.99 are no data
    
    if(dim(zt)[1] > 0){
      zt <- zt %>% mutate(depth_below_surface_ft = ifelse(depth_below_surface_ft == 999.99, NA, depth_below_surface_ft))
      zt <- zt %>% select(site, date, julian, depth_below_surface_ft) %>% rename(depth_ft = depth_below_surface_ft)
      
      year.flow <- rbind(year.flow, zt)
    }

  print(paste0(i, " is ", round(i/length(unique.dwr.sites)*100,2), "% done"))
}
bk.up.flow <- year.flow
summary(year.flow)

#account for any duplicates
year.flow <- year.flow %>% group_by(site, date, julian) %>% summarize(depth_ft = median(depth_ft, na.rm=TRUE), .groups="drop")

#merge new data with old data
year.flow <- rbind(all.data, year.flow) %>% arrange(site, date)
write.csv(year.flow, paste0(swd_html, "gw\\all_gw_levels.csv"), row.names=FALSE)


#Calculate states for all data... this takes a long while to do in tidyverse... like to see it making progress in loop
unique.sites <- unique(year.flow$site)
for (i in 1:length(unique.sites)){
  zt <- year.flow %>% filter(site==unique.sites[i]) %>% mutate(year = year(date))
  zt.stats <- zt %>% group_by(site, julian) %>% summarize(Nobs = n(), min=round(min(depth_ft, na.rm=TRUE),4), flow10 = round(quantile(depth_ft, 0.10, na.rm=TRUE),4), flow25 = round(quantile(depth_ft, 0.25, na.rm=TRUE),4),
                                                  flow50 = round(quantile(depth_ft, 0.5, na.rm=TRUE),4), flow75 = round(quantile(depth_ft, 0.75, na.rm=TRUE),4), flow90 = round(quantile(depth_ft, 0.90, na.rm=TRUE),4), 
                                                  max = round(max(depth_ft, na.rm=TRUE),4), .groups="drop")

  zt.stats <- zt.stats %>% mutate(startYr = min(zt$year), endYr = max(zt$year)) %>% dplyr::select(site, julian, min, flow10, flow25, flow50, flow75, flow90, max, Nobs, startYr, endYr)
  zt.stats$date2 <- as.Date(zt.stats$julian, origin=paste0(current.year,"-01-01"))
  zt.stats$date <- format(zt.stats$date2, format="%b-%d")
  #if(dim(zt.stats)[1] == 366) {zt.stats$date = julian$month.day366}
  #if(dim(zt.stats)[1] < 366) { 
  #    zt.stats <- merge(zt.stats, julian[,c("julian", "month.day365")], by.x="julian", by.y="julian", all.x=TRUE)   
  #    zt.stats <- zt.stats %>% rename(date = month.day365)
  #} #assumes 365 days... could be wrong
  stats <- rbind(stats, zt.stats)
  print(paste(i, "is ", round(i/length(unique.sites)*100,2), "percent done"))
}
bk.up <- stats;

is.na(stats) <- sapply(stats, is.infinite)
summary(stats)
#stats <- stats %>% mutate(date2 = as.Date(paste0(current.year,"-",date), format="%Y-%b-%d")) %>% as.data.frame()

head(stats)

#remove sites that have not had new data in last year
remove.site <- stats %>% filter(endYr < (current.year-1))  %>% select(site) %>% distinct()

######################################################################################################################################################################
#
# CREATE FILES FOR WEBSITE
#
#####################################################################################################################################################################
#Now attach most recent value to stream stats
recent.flow <- year.flow %>% group_by(site) %>% filter(date == max(date)) %>% filter(site %notin% remove.site$site) #%>% rename(flow = depth_below_surface_ft)
recent.flow <- recent.flow %>% filter(julian <= as.POSIXlt(today(), format = "%Y-%m-%d")$yday) 
current.stat <- merge(recent.flow[,c("site", "julian", "depth_ft")], stats, by.x=c("site","julian"), by.y=c("site","julian"), all.x=TRUE) 

#if else for this year and last years flow... I think flip this for gw
current.stat <- current.stat %>% mutate(status = ifelse(depth_ft <= flow10, "Extremely Wet", ifelse(depth_ft > flow10 & depth_ft <= flow25, "Very Wet", ifelse(depth_ft >= flow25 & depth_ft < flow50, "Moderately Wet", 
                                                 ifelse(depth_ft >= flow50 & depth_ft < flow75, "Moderately Dry", ifelse(depth_ft >= flow75 & depth_ft < flow90, "Very Dry", ifelse(depth_ft >= flow90, "Extremely Dry", "Unknown")))))))
current.stat$status <- ifelse(is.na(current.stat$status), "unknown", current.stat$status)
table(current.stat$status)

#set those that are not collecting data to unknown
max.julian <- current.stat %>% filter(endYr == current.year) %>% summarize(maxJ = max(julian, na.rm=TRUE))
current.stat <- current.stat %>% mutate(status = ifelse(endYr < current.year & julian > 30, "unknown", ifelse(endYr < (current.year-1), "unknown", 
                                 ifelse(endYr==current.year & julian < (max.julian$maxJ-60), "unknown", status))))
table(current.stat$status, useNA="ifany")

#merge to sites geojson
nc.sites2 <- merge(nc.sites, current.stat[,c("site","status","depth_ft","julian","date","flow50")], by.x="site", by.y="site") %>% distinct()
#convert to sf
nc.sites2 <- st_as_sf(nc.sites2, coords = c("DecLongVa", "DecLatVa"), crs = 4326);
nc.sites2 <- merge(nc.sites2 %>% dplyr::select(-date), recent.flow[,c("site","date")], by.x="site", by.y="site", all.x=TRUE)
#Save out triangle points
nc.sites2 <- nc.sites2 %>% dplyr::select(site, AgencyCd, SiteName, AltVa, WellDepth, NatAqfrDesc, LocalAquiferName, AquiferType, link, status, depth_ft, julian, flow50, date, geometry)
geojson_write(nc.sites2, file=paste0(swd_html, "gw\\gw_sites.geojson"))

#plot for fun
nc.sites2 <- nc.sites2 %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", ifelse(status=="Very Dry", "red", ifelse(status=="Moderately Dry", "orange", ifelse(status=="Moderately Wet", "cornflowerblue",
                              ifelse(status=="Very Wet", "blue", ifelse(status=="Extremely Wet", "navy", "gray")))))))
leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% 
  addCircleMarkers(data = nc.sites2, radius=4, fillOpacity= 0.8, fillColor = nc.sites2$colorStatus, color="black", weight=0) 


#Now clip time series data to past two years and assign a depth based on stats
year.flow2 <- year.flow %>% filter(date >= as.Date(paste0((current.year-2),"-01-01"), "%Y-%m-%d"))
stats2 <- merge(year.flow2[,c("site", "julian", "date", "depth_ft")], stats %>% dplyr::select(-date), by.x=c("site","julian"), by.y=c("site", "julian"), all.x=TRUE) %>% arrange(site, date)

stats2 <- stats2 %>% mutate(status = ifelse(depth_ft <= flow10, "Extremely Wet", ifelse(depth_ft > flow10 & depth_ft <= flow25, "Very Wet", ifelse(depth_ft >= flow25 & depth_ft < flow50, "Moderately Wet", 
                                           ifelse(depth_ft >= flow50 & depth_ft < flow75, "Moderately Dry", ifelse(depth_ft >= flow75 & depth_ft < flow90, "Very Dry", ifelse(depth_ft >= flow90, "Extremely Dry", "Unknown")))))))
stats2$status <- ifelse(is.na(stats2$status), "unknown", stats2$status)
table(stats2$status, useNA="ifany")
stats2 <- stats2 %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", ifelse(status=="Very Dry", "red", ifelse(status=="Moderately Dry", "orange", ifelse(status=="Moderately Wet", "cornflowerblue",
                            ifelse(status=="Very Wet", "blue", ifelse(status=="Extremely Wet", "navy", "gray")))))))
stats2 <- stats2 %>% dplyr::select(site, julian, date, depth_ft, status, colorStatus) %>% filter(site %in% nc.sites$site)
write.csv(stats2, paste0(swd_html, "gw\\gw_levels_time.csv"), row.names=FALSE)


#set up month names and save out stats file
my.month.name <- Vectorize(function(n) c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov", "Dec")[n])
recent.flow <- year.flow %>% group_by(site) %>% filter(date >= max(as.Date(paste0(current.year, "-01-01"), '%Y-%m-%d'))) 
stats.merge <- stats %>% mutate(date3 = date2, date2 = date) %>% dplyr::select(-date) %>% filter(site %in% nc.sites$site)
current.stat2 <- merge(recent.flow, stats.merge, by.x=c("site","julian"), by.y=c("site","julian"), all.y=TRUE)%>% filter(site %in% nc.sites2$site)

current.stat2 <- current.stat2 %>% mutate(month = my.month.name(as.numeric(substr(date,6,7)))) %>% mutate(date = date2, date2 = date3) %>% dplyr::select(-date3);  #okay to have NA for date because want chart to end there
write.csv(current.stat2, paste0(swd_html, "gw\\gw_stats.csv"), row.names=FALSE)


#let's do annual trends
gw.annual <- year.flow %>% mutate(year = year(date)) %>% group_by(site, year) %>% summarize(medianDepth = median(depth_ft, na.rm=TRUE), nobsv = n(), .groups="drop") %>% 
  filter(site %in% nc.sites2$site)
write.csv(gw.annual, paste0(swd_html, "gw\\gw_annual_level.csv"), row.names=FALSE)


#make triangle site updates
t.sites <- read.csv("..\\data\\gw\\triangle_sites.csv")
t.sites2 <- nc.sites2 %>% filter(site %in% t.sites$site)
geojson_write(t.sites2, file="..\\data\\gw\\gw_sites.geojson")

t.stats2 <- stats2 %>% filter(site %in% t.sites$site)
write.csv(t.stats2, "..\\data\\gw\\gw_levels_time.csv", row.names=FALSE)

t.current <- current.stat2 %>% filter(site %in% t.sites$site)
write.csv(t.current, "..\\data\\gw\\gw_stats.csv", row.names=FALSE)

t.annual <- gw.annual %>% filter(site %in% t.sites$site)
write.csv(t.annual, "..\\data\\gw\\gw_annual_level.csv", row.names=FALSE)



#remove files
rm(zt, gw.annual, recent.flow, current.stat, current.stat2, nc.sites2, nc.sites, usgs.sites, stats, year.flow, unique.usgs.sites, unique.dwr.sites, stats2, test, url.sites, year.flow2, zt.stats, bk.up, all.data, bk.up.flow)
