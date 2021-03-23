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
#   Set variables
#
######################################################################################################################################################################
current.date = as.Date(substr(Sys.time(),0,10),"%Y-%m-%d")
current.year <- year(current.date)
mymonths <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"); #used below to convert numbers to abbrev
start.date = "1990-01-01"; #set for the start of the period that we want to assess
end.date = paste0(year(current.date), "-12-31")

#create a list of counties where you want data (limit to triangle counties)
county.list <- c("Alamance County", "Orange County", "Durham County", "Granville County", "Wake County", "Franklin County", "Chatham County",
                 "Lee County", "Harnett County", "Johnstown County", "Cumberland County", "Randolph County", "Moore County", "Hoke County", "Sampson County")

######################################################################################################################################################################
#
#   ACCESS GROUNDWATER DATA = MANUAL PROCESS
#
######################################################################################################################################################################
#set up base url
#https://cida.usgs.gov/ngwmn_cache/sos?request=GetCapabilities&service=SOS&acceptVersions=2.0.0&acceptedFormats=text/xml
#because of set up did a manual download of sites in NC
nc.sites <- read.csv(paste0(swd_html, "gw\\ALL_SITE_INFO.csv")); #all NAD83
nc.sites <- nc.sites %>% select(AgencyCd, SiteNo, SiteName, DecLatVa, DecLongVa, AltVa, AltUnitsNm, WellDepth, WellDepthUnitsNm, NatAquiferCd, NatAqfrDesc, StateCd, StateNm, CountyCd, CountyNm,
                                LocalAquiferCd, LocalAquiferName, SiteType, AquiferType)
#merge together
nc.sites <- nc.sites %>% filter(CountyNm %in% county.list) %>% rename(site = SiteNo)
#table(nc.sites$CountyNm)

#save out sites
write.csv(nc.sites, paste0(swd_html, "gw\\triangle_sites.csv"), row.names = FALSE)


######################################################################################################################################################################
#
# PULL IN GW LEVEL DATA DYNAMICALLY
#
#####################################################################################################################################################################
zt <- st_as_sf(nc.sites, coords = c("DecLongVa", "DecLatVa"), crs = 4326, agr = "constant")
table(nc.sites$AgencyCd)
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
  zt <- readNWISgwl(siteNumbers = unique.usgs.sites[i], startDate=start.date, endDate = end.date)
  zt <- renameNWISColumns(zt) %>% select(agency_cd, site_no, lev_dt, lev_va) %>% rename(Date = lev_dt, depth_below_surface_ft = lev_va)
  zt <- zt %>% mutate(julian = as.POSIXlt(Date, format = "%Y-%m-%d")$yday, year = year(Date))# %>% mutate(date = paste0(month(Date, label=TRUE, abbr=TRUE),"-",day(Date))); #calculates julian date as.Date(c("2007-06-22", "2004-02-13"))
  
  
  #summarize by julian
  zt.stats <- zt %>% group_by(julian) %>% summarize(Nobs = n(), min=round(min(depth_below_surface_ft, na.rm=TRUE),4), flow10 = round(quantile(depth_below_surface_ft, 0.10, na.rm=TRUE),4), flow25 = round(quantile(depth_below_surface_ft, 0.25, na.rm=TRUE),4),
                                                    flow50 = round(quantile(depth_below_surface_ft, 0.5, na.rm=TRUE),4), flow75 = round(quantile(depth_below_surface_ft, 0.75, na.rm=TRUE),4), flow90 = round(quantile(depth_below_surface_ft, 0.90, na.rm=TRUE),4), 
                                                    max = round(max(depth_below_surface_ft, na.rm=TRUE),4),
                                                    .groups="keep")
  zt.stats <- zt.stats %>% mutate(site = as.character(unique.usgs.sites[i]), startYr = min(zt$year), endYr = max(zt$year)) %>% select(site, julian, min, flow10, flow25, flow50, flow75, flow90, max, Nobs, startYr, endYr)
  if(dim(zt.stats)[1] == 366) {zt.stats$date = julian$month.day366}
  if(dim(zt.stats)[1] < 366) { zt.stats <- merge(zt.stats, julian[,c("julian", "month.day365")], by.x="julian", by.y="julian", all.x=TRUE)   
    zt.stats <- zt.stats %>% rename(date = month.day365)
  } #assumes 365 days... could be wrong
  
  #fill dataframe
  stats <- rbind(stats, zt.stats)
  zt <- zt %>% select(site_no, Date, julian, depth_below_surface_ft);    colnames(zt) <- c("site", "date", "julian", "depth_ft")
  year.flow <- rbind(year.flow, zt)
  
  print(i)
}
#if inifinite value because of 1 observation... 
is.na(stats) <- sapply(stats, is.infinite)
summary(stats)
summary(year.flow)


############################################     RUN FOR NCDWR   #####################################################################################################
#url_base <- "https://www.ncwater.org/Data_and_Modeling/Ground_Water_Databases/potmaps/gwbdatafiles/"   #m53l1115239lev.txt"
#                 ONLY HAS DATA FOR LAST TWO YEARS
#
#########################################################################################################################################################################
url.sites <- dwr.sites %>% mutate(site2 = site) %>%  separate(site, into = c("text", "num", "text2"), sep = "(?<=[A-Za-z])(?=[0-9])(?<=[A-Za-z])") %>% mutate(url_site = paste0(text,"**",num,text2))
url.sites$link = NA

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
print(url.sites$link[i])
}  

nc.sites <- merge(nc.sites, url.sites[,c("site2","link")], by.x="site", by.y="site2", all.x=TRUE)
#head(nc.sites)
write.csv(nc.sites, paste0(swd_html, "gw\\triangle_sites.csv"), row.names = FALSE)


#Build on USGS dataframe
#Loop through each site and calculate statistics
for (i in 1:length(unique.dwr.sites)){
  zt.site <- url.sites[i,]$site2
  zt <- read.csv(url.sites$link[i], header=FALSE, sep="\t")
    colnames(zt) <- c("date", "depth_below_surface_ft", "elevation")
    zt <- zt %>% mutate(julian = as.POSIXlt(date, format = "%Y-%m-%d")$yday, year = year(date), site = zt.site)
    #999.99 are no data
    zt <- zt %>% mutate(depth_below_surface_ft = ifelse(depth_below_surface_ft == 999.99, NA, depth_below_surface_ft))
    #summarize by julian
    zt.stats <- zt %>% group_by(julian) %>% summarize(Nobs = n(), min=round(min(depth_below_surface_ft, na.rm=TRUE),4), flow10 = round(quantile(depth_below_surface_ft, 0.10, na.rm=TRUE),4), flow25 = round(quantile(depth_below_surface_ft, 0.25, na.rm=TRUE),4),
                                                      flow50 = round(quantile(depth_below_surface_ft, 0.5, na.rm=TRUE),4), flow75 = round(quantile(depth_below_surface_ft, 0.75, na.rm=TRUE),4), flow90 = round(quantile(depth_below_surface_ft, 0.90, na.rm=TRUE),4), 
                                                      max = round(max(depth_below_surface_ft, na.rm=TRUE),4),
                                                      .groups="keep")
    zt.stats <- zt.stats %>% mutate(site = zt.site, startYr = min(zt$year), endYr = max(zt$year)) %>% select(site, julian, min, flow10, flow25, flow50, flow75, flow90, max, Nobs, startYr, endYr)
    if(dim(zt.stats)[1] == 366) {zt.stats$date = julian$month.day366}
    if(dim(zt.stats)[1] < 366) { zt.stats <- merge(zt.stats, julian[,c("julian", "month.day365")], by.x="julian", by.y="julian", all.x=TRUE)   
        zt.stats <- zt.stats %>% rename(date = month.day365)
    } #assumes 365 days... could be wrong
    
    #fill dataframe
    stats <- rbind(stats, zt.stats)
    zt <- zt %>% select(site, date, julian, depth_below_surface_ft) %>% rename(depth_ft = depth_below_surface_ft)
    #zt <- zt %>% select(site, date, julian, depth_below_surface_ft);
    year.flow <- rbind(year.flow, zt)

  print(i)
}
#if inifinite value because of 1 observation... 
is.na(stats) <- sapply(stats, is.infinite)
unique(stats$site)
summary(stats)
summary(year.flow)
write.csv(year.flow, paste0(swd_html, "gw\\all_gw_levels.csv"), row.names=FALSE)

##############################################################################################################################################################################
#
#           NOW CLEAN AND PREP FILES FOR DASHBOARD
#
#########################################################################################################################################################################

stats <- stats %>% mutate(date2 = as.Date(paste0(current.year,date), "%Y-%b-%d")) %>% as.data.frame()

#set up data frame for stats and include year
gw.l <- year.flow %>% group_by(site, date) %>% summarize(depth_ft = median(depth_ft, na.rm=TRUE), .groups="drop")
gw.l <- gw.l %>% mutate(julian = as.POSIXlt(date, format = "%Y-%m-%d")$yday); #calculates julian date

gw.stats <- merge(stats, julian[,c("julian","month.day366")], by.x="julian", by.y="julian", all.x=TRUE)
#gw.stats <- gw.stats %>% mutate(date = ifelse(is.na(date), month.day366, date))

#Create a date for the webpage... notice that Feb 29th will be NA
gw.stats <- gw.stats %>% mutate(date2 = as.Date(paste0(current.year,"-",date), "%Y-%b-%d")) %>% arrange(site, julian) %>% as.data.frame()


######################################################################################################################################################################
#
# CREATE FILES FOR WEBSITE
#
#####################################################################################################################################################################
#Now attach most recent value to stream stats
recent.flow <- gw.l %>% group_by(site) %>% filter(date == max(date)) #%>% rename(flow = depth_below_surface_ft)
current.stat <- merge(recent.flow[,c("site", "julian", "depth_ft")], gw.stats, by.x=c("site","julian"), by.y=c("site","julian"), all.x=TRUE) 

#if else for this year and last years flow... I think flip this for gw
current.stat <- current.stat %>% mutate(status = ifelse(depth_ft <= flow10, "Extremely Wet", ifelse(depth_ft > flow10 & depth_ft <= flow25, "Very Wet", ifelse(depth_ft >= flow25 & depth_ft < flow50, "Moderately Wet", 
                                                 ifelse(depth_ft >= flow50 & depth_ft < flow75, "Moderately Dry", ifelse(depth_ft >= flow75 & depth_ft < flow90, "Very Dry", ifelse(depth_ft >= flow90, "Extremely Dry", "Unknown")))))))
current.stat$status <- ifelse(is.na(current.stat$status), "unknown", current.stat$status)
table(current.stat$status)

#merge to sites geojson
nc.sites2 <- merge(nc.sites, current.stat[,c("site","status","depth_ft","julian","date","flow50")], by.x="site", by.y="site")
#convert to sf
nc.sites2 <- st_as_sf(nc.sites2, coords = c("DecLongVa", "DecLatVa"), crs = 4326);
mapview::mapview(nc.sites2)
nc.sites2 <- merge(nc.sites2 %>% select(-date), recent.flow[,c("site","date")], by.x="site", by.y="site", all.x=TRUE)
#Save out triangle points
geojson_write(nc.sites2, file=paste0(swd_html, "gw\\gw_sites.geojson"))

#plot for fun
nc.sites2 <- nc.sites2 %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", ifelse(status=="Very Dry", "red", ifelse(status=="Moderately Dry", "orange", ifelse(status=="Moderately Wet", "cornflowerblue",
                              ifelse(status=="Very Wet", "blue", ifelse(status=="Extremely Wet", "navy", "gray")))))))
leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% 
  addCircleMarkers(data = nc.sites2, fillOpacity= 0.8, fillColor = nc.sites2$colorStatus, color="black", weight=0) 


#Now clip time series data to past two years and assign a depth based on stats
year.flow2 <- gw.l %>% filter(date >= as.Date(paste0((current.year-2),"-01-01"), "%Y-%m-%d"))
stats2 <- merge(year.flow2[,c("site", "julian", "date", "depth_ft")], gw.stats %>% select(-date), by.x=c("site","julian"), by.y=c("site", "julian"), all.x=TRUE) %>% arrange(site, date)

stats2 <- stats2 %>% mutate(status = ifelse(depth_ft <= flow10, "Extremely Wet", ifelse(depth_ft > flow10 & depth_ft <= flow25, "Very Wet", ifelse(depth_ft >= flow25 & depth_ft < flow50, "Moderately Wet", 
                                           ifelse(depth_ft >= flow50 & depth_ft < flow75, "Moderately Dry", ifelse(depth_ft >= flow75 & depth_ft < flow90, "Very Dry", ifelse(depth_ft >= flow90, "Extremely Dry", "Unknown")))))))
stats2$status <- ifelse(is.na(stats2$status), "unknown", stats2$status)
table(stats2$status, useNA="ifany")
stats2 <- stats2 %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", ifelse(status=="Very Dry", "red", ifelse(status=="Moderately Dry", "orange", ifelse(status=="Moderately Wet", "cornflowerblue",
                            ifelse(status=="Very Wet", "blue", ifelse(status=="Extremely Wet", "navy", "gray")))))))
stats2 <- stats2 %>% select(site, julian, date, depth_ft, status, colorStatus)
write.csv(stats2, paste0(swd_html, "gw\\gw_levels_time.csv"), row.names=FALSE)


#set up month names and save out stats file
my.month.name <- Vectorize(function(n) c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov", "Dec")[n])
recent.flow <- gw.l %>% group_by(site) %>% filter(date >= max(as.Date(paste0(current.year, "-01-01"), '%Y-%m-%d'))) 
current.stat2 <- merge(recent.flow, gw.stats %>% select(-date), by.x=c("site","julian"), by.y=c("site","julian"), all.y=TRUE) 
current.stat2 <- current.stat2 %>% mutate(month = my.month.name(as.numeric(substr(date2,6,7)))) %>% mutate(date = month.day366) %>% select(-month.day366)
write.csv(current.stat2, paste0(swd_html, "gw\\gw_stats.csv"), row.names=FALSE)


#let's do annual trends
gw.annual <- gw.l %>% mutate(year = year(date)) %>% group_by(site, year) %>% summarize(medianDepth = median(depth_ft, na.rm=TRUE), nobsv = n(), .groups="drop")
write.csv(gw.annual, paste0(swd_html, "gw\\gw_annual_level.csv"), row.names=FALSE)


#remove files
rm(zt, gw.annual, recent.flow, current.stat, current.stat2, gw.stats, nc.sites2, nc.sites, usgs.sites, stats, year.flow, usgs.sites, dwr.sites, stats2, test, url.sites, year.flow2, zt.stat, a.test, zt.stats)
