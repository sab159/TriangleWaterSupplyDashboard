#######################################################################################################################################################
#
# Updates precipitation data based on historic data collected
# also updates forecast maps
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# Can be updated daily
# FEBRUARY 2021
#
########################################################################################################################################################


################################################################################################################################################
#
#                      UPDATE THE DROUGHT MAPS AND CREATE TABLE FOR PERCENT IN DROUGHT BY BASIN
#
################################################################################################################################################
#download spatial data
drought <- file_to_geojson(input="https://droughtmonitor.unl.edu/data/kmz/usdm_current.kmz", method='web', output=paste0(swd_html, 'drought\\current_drought'))
drought <- read_sf(paste0(swd_html, 'drought\\current_drought.geojson')) %>%  st_transform(drought, crs = 4326) %>% select(Name, geometry); #already in correct projection

#download tables for HUCS of interest 
huc8 <- read_sf(paste0(swd_html, "huc8.geojson"))
huc.list <- huc8$huc8

#loop through the lists to create a data frame... it takes a lont time to read through. Pull in oriinal and add to it
old.drought <- read.csv(paste0(swd_html, "drought\\percentAreaHUC.csv"), colClasses=c("huc8" = "character")) %>% mutate(date = as.Date(date, "%Y-%m-%d")) 
last.date <- max(old.drought$date)
#reformat for the url (1/1/2020)
last.day <- day(last.date)+1; last.month = month(last.date); last.year = year(last.date)
last.date <- paste0(last.month,"/",last.day,"/",last.year)
end.date <- paste0("12/31/", year(today))

#create dataframe and pull new data
drought.time <- as.data.frame(matrix(nrow=0, ncol=9)); colnames(drought.time) <- c("huc8","name","date","none","d0","d1","d2","d3","d4")
for (m in 1:length(huc.list)){
  full_url <-paste0("https://usdmdataservices.unl.edu/api/HUCStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=",huc.list[m],"&startdate=",last.date,"&enddate=", end.date, "&statisticsType=1")
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

#combine wiht old drought and remove duplicates
drought2 <- rbind(old.drought, drought2)
drought2 <- drought2 %>% arrange(huc8, date) %>% distinct()

#save file
write.csv(drought2, paste0(swd_html, "drought\\percentAreaHUC.csv"), row.names=FALSE)
rm(drought, drought2, zt.name, zt, last.month, last.year, last.day, last.date, full_url, api.data, drought.time, df, old.drought, m, huc.list, i)
###################################################################################################################################################################################################################################
#
###################################################################################################################################################################################################################################



################################################################################################################################################
#
#                      UPDATE THE FORECAST DATA PROVIDED BY NOAA
#
################################################################################################################################################
# CRS definition for HRAP projection, details and reference below
crs.hrap <- CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-105 +x_0=0 +y_0=0 +R=6371200 +units=m +no_defs'); #have to set projection to read in; https://gist.github.com/tcmoran/a3bb702f14a1b45c1bd3

#create the url to obtain the last 7 days of observed precipitation and percent of normal precipitation
#Sometimes the day needs to be one or two earlier for the file to exist
year.url <- year(Sys.Date()); month.url <- month(Sys.Date()); day.url <- day(Sys.Date())
if(nchar(month.url)==1) { month.url = paste0("0", month.url) }
if(nchar(day.url)==1) { day.url = paste0("0", day.url) }

url.used <- paste0("https://water.weather.gov/precip/downloads/",year.url,"/",month.url,"/",day.url,"/nws_precip_last7days_",year.url,month.url,day.url,"_conus.tif")
#call data in as a raster
zt <- raster(url.used)
#the data are povided as 4 bands in one raster. We are interested in Band 1 and Band 4
  #Band 1 - Observation - Last 24 hours of QPE spanning 12Z to 12Z in inches
  #Band 2 - PRISM normals - PRISM normals in inches (see "Normal Precipitation" section on the About page)
  #Band 3 - Departure from normal - The departure from normal in inches
  #Band 4 - Percent of normal - The percent of normal
zt1 <- raster(url.used, band=1);
zt4 <- raster(url.used, band=4)

#clip zt2 to huc
zt1.proj <- projectRaster(zt1, crs="+proj=longlat +datum=WGS84")
zt1.proj <- crop(zt1.proj, extent(huc8));    #zt1.proj <- mask(zt1.proj, huc); #extent makes a box, while mask clips to huc
  mapview::mapview(zt1.proj)
#NA value is -10000
pol <- rasterToPolygons(zt1.proj); colnames(pol@data) <- c("obsv_in")
pol <- pol %>% st_as_sf() %>% mutate(obsv_in = round(obsv_in,2)) %>% ms_simplify(keep = 0.5, keep_shapes=TRUE); #convert to a geojson and simplify to plot faster

#summarize and dissolve based on ranges
pol2 <- pol %>% mutate(bands = ifelse(obsv_in == 0, 0, ifelse(obsv_in <=0.1 & obsv_in > 0, 0.1, ifelse(obsv_in <=0.25 & obsv_in > 0.1, 0.25, ifelse(obsv_in <=0.5 & obsv_in > 0.25, 0.50, ifelse(obsv_in <=1 & obsv_in > 0.5, 1,
                               ifelse(obsv_in <=2 & obsv_in > 1,2, ifelse(obsv_in <=3 & obsv_in > 2, 3, ifelse(obsv_in <=4 & obsv_in > 3, 4, ifelse(obsv_in <=5 & obsv_in > 4, 5, ifelse(obsv_in <=6 & obsv_in > 5, 6, 
                                ifelse(obsv_in <=8 & obsv_in > 6,8, ifelse(obsv_in <=10 & obsv_in > 8, 10, ifelse(obsv_in <=15 & obsv_in > 10, 15, ifelse(obsv_in <20 & obsv_in > 15, 20, ifelse(obsv_in > 20,30, NA))))))))))))))))
table(pol2$bands, useNA="ifany")
pol2 <- pol2 %>% group_by(bands) %>% summarize(nbands = n(), .groups="drop")
pol2 <- pol2 %>% mutate(colorVal = ifelse(bands==0, "white", ifelse(bands==0.1, "#3fc1bf", ifelse(bands==0.25, "#87b2c0", ifelse(bands==0.5, "#000080", ifelse(bands==1, "#00fc02", ifelse(bands==1.5, "#56b000", 
                                   ifelse(bands==2, "#316400", ifelse(bands==3, "yellow", ifelse(bands==4, "#f7e08b", ifelse(bands==5, "orange", ifelse(bands==6, "red", ifelse(bands==8, "#9a0000",
                                   ifelse(bands==10, "#4e0000", ifelse(bands==15, "#e00079", ifelse(bands>=20,"#8e2eff", "black"))))))))))))))))
#convert pol to geojson and simplify
leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = pol2, fillOpacity= 0.8, fillColor = pol2$colorVal, color="black", weight=0)
#write 7 day observations to file
geojson_write(pol2, file =  paste0(swd_html, "pcp\\pcp_7day_obsv.geojson"))


#Redo for percent of normal
zt4.proj <- projectRaster(zt4, crs="+proj=longlat +datum=WGS84")
zt4.proj <- crop(zt4.proj, extent(huc8));     #zt4 proj <- mask(zt4.proj, huc)

pol <- rasterToPolygons(zt4.proj); colnames(pol@data) <- c("percent_norm");
pol <- pol %>% st_as_sf() %>% mutate(percent_norm = round(percent_norm,2)) %>% ms_simplify(keep = 0.5, keep_shapes=TRUE)

#summarize and dissolve based on ranges
pol2 <- pol %>% mutate(bands = ifelse(percent_norm == 0, 0, ifelse(percent_norm <=5 & percent_norm > 0, 5, ifelse(percent_norm <=10 & percent_norm > 5, 10, ifelse(percent_norm <=25 & percent_norm > 10, 25, 
                               ifelse(percent_norm <=50 & percent_norm > 25, 50, ifelse(percent_norm <=75 & percent_norm > 50, 75, ifelse(percent_norm <=90 & percent_norm > 75, 90, ifelse(percent_norm <=100 & percent_norm > 90, 100,
                               ifelse(percent_norm <=110 & percent_norm > 100, 110, ifelse(percent_norm <=125 & percent_norm > 110, 125, ifelse(percent_norm <=150 & percent_norm > 125, 150, 
                               ifelse(percent_norm <=200 & percent_norm > 150, 200, ifelse(percent_norm <=300 & percent_norm > 200, 300, ifelse(percent_norm <= 400 & percent_norm > 300, 400, 
                               ifelse(percent_norm > 400 & percent_norm <=600, 600, ifelse(percent_norm > 600, 800, NA)))))))))))))))))
table(pol2$bands, useNA="ifany")
pol2 <- pol2 %>% group_by(bands) %>% summarize(nbands = n(), .groups="drop")
pol2 <- pol2 %>% mutate(colorVal = ifelse(bands==0, "white", ifelse(bands==5, "#4e0000", ifelse(bands==10, "#9a0000", ifelse(bands==25, "red", ifelse(bands==50, "orange", ifelse(bands==75, "#f7e08b", 
                 ifelse(bands==90, "yellow", ifelse(bands==100, "#316400", ifelse(bands==110, "#00fc02", ifelse(bands==125, "#56b000", ifelse(bands==150, "#316400", ifelse(bands==200, "#3fc1bf",
                 ifelse(bands==300, "#000080", ifelse(bands==400, "#8e2eff", ifelse(bands>400,"#e00079", "black"))))))))))))))))

leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>%   addPolygons(data = pol2, fillOpacity= 0.8, fillColor = pol2$colorVal, color="black", weight=0)
geojson_write(pol2, file =  paste0(swd_html, "pcp\\pcp_7day_percent_normal.geojson"))
#clear out files
rm(zt, zt1, zt4, pol, pol2, zt1.proj, zt4.proj)



###################################################################################################################################
#          6-10 day precipitation and temperature outlooks
####################################################################################################################################
#there is a one day lag
day.url <- day(Sys.Date())-1
if(nchar(day.url)==1) { day.url = paste0("0", day.url) }

#read in pcp data
file_to_geojson(input=paste0("ftp://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/610prcp_",year.url,month.url,day.url,".kmz"), method='web', output=paste0(swd_html, 'pcp\\pcp610forecast'))
pcp <- read_sf(paste0(swd_html, 'pcp\\pcp610forecast.geojson')) %>% dplyr::select(Name, geometry) %>% st_transform(crs = 4326)
pcp <- pcp %>% mutate(percentage = as.numeric(trimws(substr(Name,0,3))), direction = trimws(substr(Name, (nchar(Name)-12), (nchar(Name)-7)),"both"))
pcp <- pcp %>% mutate(colorVal = ifelse(percentage < 33, "white", "black")) %>% mutate(colorVal = ifelse(direction == "Above" & percentage >= 33 & percentage < 40, "#d4f8d4", colorVal)) %>% 
  mutate(colorVal = ifelse(direction == "Above" & percentage >= 40 & percentage < 50, "#90ee90", ifelse(direction == "Above" & percentage >= 50 & percentage < 60, "#4ce44c", 
                    ifelse(direction == "Above" & percentage >= 60 & percentage < 70, "#1ec31e", ifelse(direction == "Above" & percentage >= 70 & percentage < 80, "#169016", 
                    ifelse(direction == "Above" & percentage >= 80 & percentage <= 100, "#0c4c0c", colorVal))))))

pcp <- pcp %>% mutate(colorVal = ifelse(direction == "Below" & percentage >= 33 & percentage < 40, "#e1d9d2", ifelse(direction == "Below" & percentage >= 40 & percentage < 50, "#b9a797", 
                                 ifelse(direction == "Below" & percentage >= 50 & percentage < 60, "#b19d8c", ifelse(direction == "Below" & percentage >= 60 & percentage < 70, "#776250", 
                                 ifelse(direction == "Below" & percentage >= 70 & percentage < 80, "#5f4f40", ifelse(direction == "Below" & percentage >= 80 & percentage <= 100, "#312821", colorVal)))))))
pcp <- pcp %>% mutate(colorVal = ifelse(direction == "Normal", "white", colorVal))
pcp <- st_zm(pcp)
table(pcp$colorVal)
leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = pcp, fillOpacity= 0.75, fillColor = pcp$colorVal, color="black", weight=0)
geojson_write(pcp, file =  paste0(swd_html, "pcp\\pcp610forecast.geojson"))

#repeat for temperature------------------------
file_to_geojson(input=paste0("ftp://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/610temp_",year.url,month.url,day.url,".kmz"), method='web', output=paste0(swd_html, 'pcp\\temp610forecast'))
pcp <- read_sf(paste0(swd_html, 'pcp\\temp610forecast.geojson')) %>% dplyr::select(Name, geometry) %>% st_transform(crs = 4326)
pcp <- pcp %>% mutate(percentage = as.numeric(trimws(substr(Name,0,3))), direction = trimws(substr(Name, (nchar(Name)-12), (nchar(Name)-7)),"both"))
pcp <- pcp %>% mutate(colorVal = ifelse(percentage < 33, "white", "black")) %>% mutate(colorVal = ifelse(direction == "Above" & percentage >= 33 & percentage < 40, "#ffc4c4", colorVal)) %>% 
  mutate(colorVal = ifelse(direction == "Above" & percentage >= 40 & percentage < 50, "#ff7676", ifelse(direction == "Above" & percentage >= 50 & percentage < 60, "#ff2727", 
                    ifelse(direction == "Above" & percentage >= 60 & percentage < 70, "#eb0000", ifelse(direction == "Above" & percentage >= 70 & percentage < 80, "#b10000", 
                    ifelse(direction == "Above" & percentage >= 80 & percentage <= 100, "#760000", colorVal))))))

pcp <- pcp %>% mutate(colorVal = ifelse(direction == "Below" & percentage >= 33 & percentage < 40, "#d8d8ff", ifelse(direction == "Below" & percentage >= 40 & percentage < 50, "#9d9dff", 
                                 ifelse(direction == "Below" & percentage >= 50 & percentage < 60, "#4e4eff", ifelse(direction == "Below" & percentage >= 60 & percentage < 70, "#1414ff", 
                                 ifelse(direction == "Below" & percentage >= 70 & percentage < 80, "#0000d8", ifelse(direction == "Below" & percentage >= 80 & percentage <= 100, "#00009d", colorVal)))))))
pcp <- pcp %>% mutate(colorVal = ifelse(direction == "Normal", "white", colorVal))
pcp <- st_zm(pcp)
table(pcp$colorVal)
leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = pcp, fillOpacity= 0.6, fillColor = pcp$colorVal, color="black", weight=0)
geojson_write(pcp, file =  paste0(swd_html, "pcp\\temp610forecast.geojson"))

rm(pcp)

###################################################################################################################################
#
#          1-7 day total precipitation forecast amount
#
###################################################################################################################################
file_to_geojson(input="https://www.wpc.ncep.noaa.gov/kml/qpf/QPF168hr_Day1-7_latest.kmz", method='web', output= paste0(swd_html, 'pcp\\qpf1-7dayforecast'))
pcp <- read_sf(paste0(swd_html, 'pcp\\qpf1-7dayforecast.geojson'))# %>% dplyr::select(Name, geometry) %>% st_transform(crs = 4326)

pcp2 <- st_crop(pcp, extent(huc8)) 

#add colors
pcp2 <- pcp %>% rename(bands = Name) %>% dplyr::select(bands, geometry) %>% 
  mutate(colorVal = ifelse(bands==0, "white", ifelse(bands==0.01, "lightgray",ifelse(bands==0.1, "#228b22", ifelse(bands==0.25, "#2cb42c", ifelse(bands==0.5, "#000080", #greens
                    ifelse(bands==0.75, "#000072",  ifelse(bands==1, "#005fbf",  ifelse(bands==1.25, "#007cfa", ifelse(bands==1.5, "#00bfbf", #blues
                    ifelse(bands==1.75, "#9370db", ifelse(bands==2, "#663399", ifelse(bands==2.5, "#800080", #purples
                    ifelse(bands==3, "darkred", ifelse(bands==4, "red", ifelse(bands==5, "#ff4500", ifelse(bands==7, "orange", #red/orange
                    ifelse(bands==10, "#8b6313",ifelse(bands==15, "#daa520",ifelse(bands<=20,"yellow", "black"))))))))))))))))))))
mapview::mapview(pcp2)
#pcp2 <- pcp2 %>% ms_simplify(0.5, keep_shapes=TRUE)
geojson_write(pcp2, file =  paste0(swd_html, "pcp\\qpf1-7dayforecast.geojson"))

rm(pcp, pcp2)
###################################################################################################################################################################################################################################
#
###################################################################################################################################################################################################################################




###################################################################################################################################
#
#          READ IN LIST OF STATIONS - MANUALLY CREATED HERE: https://api.climate.ncsu.edu/locations
#          Because there is an api limit, it is very important to call the minimum amount
#
###################################################################################################################################
nc.loc <- read.csv(paste0(swd_html, "pcp\\ncsu_triangle_locations.csv"))
old.pcp <- read.csv(paste0(swd_html, "pcp\\ncsu_triangle_data.csv")) %>% mutate(date = as.Date(date, "%Y-%m-%d"))
pcp.list <- unique(old.pcp$id)
julian <- read.csv(paste0(swd_html, "julian-daymonth.csv"))
  

###################################################################################################################################
#          PULL TO BUILD DATABASE
###################################################################################################################################
#build new dataframe
nc.data <-as.data.frame(matrix(nrow=0, ncol=12)); colnames(nc.data) <- c("locID","date","var","value","unit","score","nettype","vartype","obtime","obtype","obnum","value_accum")
#count how much of data useing
total.obsv = 0;
for (i in 2:length(pcp.list)){
   #build api link
  url_loc = pcp.list[i]
  last_date = old.pcp %>% filter(id==pcp.list[i]) %>% filter(date == max(date)) %>% dplyr::select(date) %>% mutate(date = as.Date(date, "%Y-%m-%d") + 1)
    url_time_start = paste0("&start=",last_date$date)
    url_time_end = paste0("&end=",today)
  url_base = "https://api.climate.ncsu.edu/data.php?var=precip&obtype=D&output=csv&loc=location="; #for daily precipitation
  url_hash = paste0("&hash=", ncsco.key)
  
  full_url = paste0(url_base, url_loc, url_time_start, url_time_end, url_hash)  
  
  #call url
  zt <- readLines(full_url, warn=FALSE)
  #start cleaning
  yt.obsv <- as.numeric(str_split(zt[11],": ", simplify=TRUE)[1,2])
  
  #call value data
  yt.df <- read_csv(zt, comment="##")
  
  #total obs
  total.obsv = total.obsv + yt.obsv;
  nc.data <- rbind(nc.data, yt.df);

  print(paste0(i, ": ", total.obsv))
}
#save files
bk.data <- nc.data

###################################################################################################################################
#          LOOP THROUGH AND COMBINE OLD AND NEW DATA, REMOVING ANY DUPLICATE DAYS
#          ALL DATA WITH A BAD DATA SCORE REPORT 0 RAIN...so NOTHING TO DO THERE
###################################################################################################################################
#check QAQC flagged data
nc.data %>% filter(score==3) %>% as.data.frame()# keep?
table(nc.data$unit); #should all be inches
table(nc.data$nettype); #should all be measured
table(nc.data$vartype); #these are all A - aggregate of multiple variables?
table(nc.data$obtype); #these should all be d for daily


#rename columns and minimize
pcp.data <- nc.data %>% dplyr::select(location, datetime, value, value_accum) %>% mutate(value = as.numeric(value))
colnames(pcp.data) <- c("id", "date", "pcp_in", "cum_pcp")
#set NA / bad data to zero
pcp.data[is.na(pcp.data)] <-0

#convert date time to just date and add year column
pcp.data <- pcp.data %>% mutate(date = as.Date(substr(date,0,10), "%Y-%m-%d"), year = year(date), month = month(date))

#merge older data with newer data
pcp.data <- rbind(old.pcp, pcp.data)

pcp.data <- pcp.data %>% arrange(id, date) %>% distinct()
  table(pcp.data$id, pcp.data$year)

check.last.date <- pcp.data %>% group_by(id) %>% filter(is.na(pcp_in) == FALSE) %>% filter(date == max(date)) %>% dplyr::select(id, date, month)
table(check.last.date$date)

#save data
write.csv(pcp.data, paste0(swd_html, "pcp\\ncsu_triangle_data.csv"), row.names=FALSE)

###################################################################################################################################
#          CREATE TABLE FOR MONTHLY PRECIPITATION TOTALS
###################################################################################################################################
#CAn plot like demand - monthly summary
foo.month <- pcp.data %>% group_by(id, year, month) %>% summarize(pcp_in = sum(pcp_in, na.rm=TRUE), ndays = n(), .groups="drop")  %>% 
  pivot_wider(id_cols = c("id", "month"), names_from = year, names_prefix = "yr_", values_from = pcp_in) %>% arrange(id, month)
#we need to do the pivots to get NA fields in there
foo.month <- foo.month %>% pivot_longer(cols = starts_with("yr"), names_to = "year", names_prefix = "yr_", values_to = "pcp_in", values_drop_na = FALSE)
foo.month <- foo.month %>% arrange(id, year, month)
#add ndays back in
foo.m <- pcp.data %>% group_by(id, year, month) %>% summarize(ndays = n(), .groups="drop")
foo.month <- merge(foo.month, foo.m, by.x=c("id", "month", "year"), by.y=c("id", "month", "year"), all=TRUE)

#lets say you have to have ~90% of data... so 27 days... but we want to keep the current months data
  yt <- foo.month %>% filter(ndays < 27); table(yt$year, yt$month)
current.month <- month(Sys.time()); current.year <- year(Sys.time())

foo.month <- foo.month %>% mutate(pcp_in = ifelse((month == current.month & year == current.year) | ndays >=27, pcp_in, NA))
  yt <- foo.month %>% filter(is.na(pcp_in)); table(yt$year, yt$month)

#save file --- since only plotting recent years will only save out 2000 onward
#foo.month <- foo.month %>% filter(year>=1997)
write.csv(foo.month, paste0(swd_html, "pcp\\pcp_months_total.csv"), row.names=FALSE)


###################################################################################################################################
#
#          CREATE TABLE FOR CUMULATIVE PRECIPITATION TOTALS
#          CUM PCP THEY GIVE IS NOT BASED ON CALENDAR YEAR 
#
###################################################################################################################################
foo.count <- pcp.data %>% group_by(id, year) %>% count() %>% filter(year < current.year & n>340 | year == current.year) %>% mutate(idyr = paste0(id,"-",year)) 
foo.cum <- pcp.data %>% mutate(idyr = paste0(id,"-",year), day=day(date)) %>% filter(idyr %in% foo.count$idyr) %>% arrange(id, year, month, day)
foo.cum <- foo.cum %>% distinct()

foo.cum <- foo.cum %>% mutate(julian = as.POSIXlt(date, format = "%Y-%m-%d")$yday) %>% arrange(id, year, julian) %>% dplyr::select(id, year, date, julian, pcp_in) %>% distinct() %>% 
  group_by(id, year) %>%  mutate(cum_pcp = cumsum(pcp_in)) %>% dplyr::select(-pcp_in) %>% rename(pcp_in = cum_pcp)

table(foo.cum$id, foo.cum$year)
foo.cum <- foo.cum %>%   pivot_wider(id_cols = c("id", "julian"), names_from = year, names_prefix = "yr_", values_from = pcp_in) %>% arrange(id, julian) 

foo.cum <- foo.cum %>% pivot_longer(cols = starts_with("yr"), names_to = "year", names_prefix = "yr_", values_to = "pcp_in", values_drop_na = FALSE) %>% arrange(id, year, julian) %>% 
  filter(julian == 365 & is.na(pcp_in)==FALSE | julian < 365) %>% group_by(id, year) %>% mutate(ndays = n()) %>% ungroup()

#remove years with more than 30 days missing (with the exception of the current year)
foo.cum2 <- foo.cum %>% group_by(id, year) %>% mutate(nMissing = sum(is.na(pcp_in))) %>% filter(year>=2000); #shorten for this file
foo.cum2 <- foo.cum2 %>% filter(year == current.year | year < current.year & nMissing <= 31) %>% dplyr::select(-nMissing) #removes those missing more than a month of data

#add this to include in plot_ly by setting tick format to %b-%d
foo.cum2 <- merge(foo.cum2, julian[,c("julian","month.day365","month.day366")], by.x="julian", by.y="julian", all.x=TRUE) %>% arrange(id, year, julian)
foo.cum2$date = ifelse(foo.cum2$ndays==366, foo.cum2$month.day366, foo.cum2$month.day365) 
foo.cum2 <- foo.cum2 %>% dplyr::select(id, year, julian, pcp_in, date)

write.csv(foo.cum2, paste0(swd_html, "pcp\\pcp_cum_total.csv"), row.names=FALSE)



###################################################################################################################################
#          Add current status to map
###################################################################################################################################
#convert station sites into an sf file
sites <- st_as_sf(nc.loc, coords = c("long_x", "lat_y"), crs = 4326); 
mapview::mapview(sites)

sites <- sites %>% mutate(startYr = year(startDate), endYr = year(endDate)) %>% dplyr::select(locID, network, name, elev_ft, agency, startYr, endYr, geometry) %>% rename(id = locID)

#get statistics by julian day to see cumulative pcp
ytd2 <- foo.cum %>% group_by(id, julian) %>%  summarize(min = round(min(pcp_in, na.rm=TRUE),2), flow10 =  round(quantile(pcp_in, 0.10, na.rm=TRUE),2), flow25 = round(quantile(pcp_in, 0.25, na.rm=TRUE),2),
                                                    flow50 = round(quantile(pcp_in, 0.5, na.rm=TRUE),2), flow75 = round(quantile(pcp_in, 0.75, na.rm=TRUE),2), flow90 = round(quantile(pcp_in, 0.90, na.rm=TRUE),2), max = round(max(pcp_in, na.rm=TRUE),2),.groups="drop")

ytd.now <- pcp.data %>% group_by(id) %>% filter(date == max(date))  %>% mutate(julian = as.POSIXlt(date, format = "%Y-%m-%d")$yday) %>% dplyr::select(id, date, julian)
ytd.now.cum <- foo.cum %>% group_by(id) %>% filter(is.na(pcp_in) == FALSE) %>% filter(year == max(year))

ytd.now <- merge(ytd.now, ytd.now.cum, by.x=c("id", "julian"), by.y=c("id","julian"), all.x=TRUE) %>% mutate(year = year(date))
ytd.now <- merge(ytd.now, ytd2, by.x=c("id", "julian"), by.y=c("id","julian"), all.x=TRUE)

ytd.now <- ytd.now %>% mutate(status = ifelse(pcp_in <= flow10, "Extremely Dry", ifelse(pcp_in > flow10 & pcp_in <= flow25, "Very Dry", ifelse(pcp_in >= flow25 & pcp_in < flow50, "Moderately Dry", 
                                              ifelse(pcp_in >= flow50 & pcp_in < flow75, "Moderately Wet", ifelse(pcp_in >= flow75 & pcp_in < flow90, "Very Wet", ifelse(pcp_in >= flow90, "Extremely Wet", "Unknown")))))))
ytd.now <- ytd.now %>% mutate(status = ifelse(is.na(status)==TRUE, "Unknown", status)) %>%
                    mutate(status = ifelse(date <= (max(date)-10), "Unknown", status))
table(ytd.now$status, useNA="ifany")

nc.sites <- merge(sites, ytd.now[,c("id", "julian", "date", "year", "pcp_in", "status")], by.x="id", by.y="id", all=TRUE)
geojson_write(nc.sites, file = paste0(swd_html, "pcp\\pcp_sites.geojson"))



