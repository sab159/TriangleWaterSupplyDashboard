###################################################################################################################################################
#
#      CODE TO READ IN SENSOR OF THINGS DATA FOR NC WATER SUPPLY DASHBOARD
# CREATED BY LAUREN PATTERSON & KYLE ONDA @ THE INTERNET OF WATER
# FEBRUARY 2021
# RUN AFTER global0_set_apis_libraries
#
###################################################################################################################################################


######################################################################################################################################################################
#
#   LOAD Data
#
######################################################################################################################################################################
#load in geojson for utilities
utilities <- read_sf(paste0(swd_html, "nc_utilities.geojson")); 
pwsid.list <- unique(subset(utilities, data=="yes")$ncpwsid)
mymonths <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"); #used below to convert numbers to abbrev

#calculate moving average function
ma <- function(x,n=7){stats::filter(x,rep(1/n,n), sides=1)}

######################################################################################################################################################################
#
# Read in water demand data
#
#####################################################################################################################################################################
#https://twsd.internetofwater.dev/api/v1.1/Datastreams('NC0332010-WaterDistributed')/Observations?$count=true

#set up base url
url.base <- "https://twsd.internetofwater.dev/api/v1.1/Datastreams"
url.data <- "WaterDistributed"

#set up data frame
demand <- as.data.frame(matrix(nrow=0, ncol=3)); colnames(demand) = c("pwsid", "date", "demand_mgd")
#loop through the systems to call data
for(i in 1:length(pwsid.list)){
  selected.pwsid  <- pwsid.list[i] 
  old.zt <- old.demand %>% filter(pwsid == selected.pwsid)
  last.date <- max(old.demand$date2)
  
  #
  full.url <- paste0(url.base, "('", selected.pwsid, "-", url.data, "')/Observations?$count=true")
  full.url <- paste0(url.base, "('", selected.pwsid, "-", url.data, "')/Observations?$filter=phenomenonTime gt ", last.date, "T00:00:01.000Z&$count=true")
  
  #read in data
  foo <- fromJSON(full.url)
  #now loop through if additional values need to be added because
  count <- ceiling(foo$`@iot.count`/100); count #new count value
  
  for (j in 1:count){
    #pull out variables of interest
    foo <- fromJSON(full.url)
    zt <- foo$value %>% select(resultTime, result, parameters)
    zt <- zt %>% mutate(pwsid = as.character(tolower(selected.pwsid)), date = as.Date(substr(resultTime,1,10),format='%Y-%m-%d')) %>%  rename(demand_mgd = result) %>% select(pwsid, date, demand_mgd)

    #rbind to dataframe
    demand <- rbind(demand, zt)
    #set up new url in loop
    full.url <- foo$`@iot.nextLink`; #new full url
  }
  print(paste0(i, ": ", selected.pwsid))
}
summary(demand)
table(demand$pwsid)

#some of the data are daily and some are monthly... only want to do this if data are daily
demand2 <- demand %>% group_by(pwsid) %>% arrange(date) %>% mutate(timeDays = as.numeric(date - lag(date)))
demand2 <- demand2 %>% group_by(pwsid) %>% mutate(mean_demand = ifelse(timeDays <= 3, round(as.numeric(ma(demand_mgd)),2), demand_mgd), 
                                                  julian = as.numeric(strftime(date, format = "%j")), month = month(date), monthAbb = mymonths[month], year = year(date))
  #for the first 6 values that are NA - use actual demand?
demand2 <- demand2 %>% mutate(demand_mgd = round(demand_mgd,2), mean_demand = ifelse(is.na(mean_demand)==TRUE, demand_mgd, mean_demand))
  #calculate monthly peak
demand2 <- demand2 %>% group_by(pwsid, month, year) %>% mutate(peak_demand = round(quantile(demand_mgd, 0.98),1)); #took the 98% to omit outliers

#provide julian date
demand2 <- demand2 %>% mutate(date2 = date, date = paste0(monthAbb,"-",day(date2))) %>% select(-timeDays)

#write.csv
write.csv(demand2, paste0(swd_html, "demand\\demand_over_time.csv"), row.names=FALSE)
