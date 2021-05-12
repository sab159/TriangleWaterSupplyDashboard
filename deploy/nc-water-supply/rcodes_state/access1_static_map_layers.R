#######################################################################################################################################################
#
# DOWNLOADS AND CREATES GEOJSON FILES FOR MAP LAYERS IN THE NORTH CAROLINA WATER SUPPLY DASHBOARD
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# FEBRUARY 2021
#
########################################################################################################################################################



######################################################################################################################################################################
#
#   PREPARE UTILITY SHAPEFILE FOR TRIANGLE REGION
#
######################################################################################################################################################################
#create a list of pwsid list of utilities
pwsid.info <- read.csv(paste0(swd_html, "basic_info.csv"), header=TRUE) %>% select(pwsid, utility_name, data)
pwsid.list <- unique(pwsid.info$pwsid)

######################################################################################################################################################################
#
#   Create Utility Map Layer FROM GEOCONNEX
#
######################################################################################################################################################################
#https://info.geoconnex.us/collections/pws/items?ST=NC&startindex=20&limit=10&f=json
lt <- read_sf("https://info.geoconnex.us/collections/pws/items?ST=NC&startindex=20&limit=10&f=json")
lt2 <- read_sf("https://info.geoconnex.us/collections/pws/items?ST=NC&limit=10&f=json&startindex=20")

#read in water systems - note that this link may change over time... this does take longer because of files
nc.systems <- read_sf(paste0("https://info.geoconnex.us/collections/pws/items?PWSID=",pwsid.list[1]))
for(i in 2:length(pwsid.list)){
  zt <- read_sf(paste0("https://info.geoconnex.us/collections/pws/items?PWSID=",pwsid.list[i]))
  nc.systems <- rbind(nc.systems, zt)
}

nc.systems <- nc.systems %>% rename(pwsid = id) %>% select(pwsid, uri, geometry)
nc.systems  <- merge(nc.systems, pwsid.info, by="pwsid")
#simplify and reduce size
nc.systems <- ms_simplify(nc.systems, keep = 0.1, keep_shapes=TRUE)
mapview::mapview(nc.systems)
geojson_write(nc.systems, file =  paste0(swd_html, "nc_utilities.geojson"))


#Read in all water systems
baseURL =  "https://aboutus.internetofwater.dev/geoserver/ows?service=WFS&version=1.0.0&request=GetFeature&typename=geonode%3APWS_NC_20190&outputFormat=json&srs=EPSG%3A2264&srsName=EPSG%3A2264"
nc.systems2 <- read_sf(baseURL)
#limit to only those utilities in the pilot
nc.systems <- nc.systems2 %>% st_transform(crs = 4326) %>% select(PWSID, SystemName.x) %>% mutate(ncpwsid = paste0("NC", str_remove_all(PWSID, "[-]"))) %>% 
  mutate(pwsid = str_remove_all(ncpwsid, "[_]")) %>% rename(utility_name=SystemName.x); #one of the pwsid has a data entry error
nc.systems <- merge(nc.systems, pwsid.info, by.x="pwsid", by.y="pwsid", all.x=TRUE); #merge to get preferred utility names into the shapefile
#nc.systems <- nc.systems %>% select(PWSID, ncpwsid, utility_name, data)
nc.systems <- nc.systems %>% mutate(utility_name = ifelse(is.na(utility_name.y), utility_name.x, utility_name.y), data = ifelse(is.na(data), "no", data)) %>% 
                                      select(pwsid, utility_name, data, geometry)
nc.systems <- ms_simplify(nc.systems, keep = 0.1, keep_shapes=TRUE)

#add system names
mapview::mapview(nc.systems)
geojson_write(nc.systems, file =  paste0(swd_html, "nc_utilities.geojson"))


  
######################################################################################################################################################################
#
#   Create STATIC MAP LAYERS FROM GEOCONNEX
#
######################################################################################################################################################################
#read in state data
state <- read_sf(paste0("https://info.geoconnex.us/collections/states/items?STUSPS=",stateAbb))
#read in county data
county <- read_sf(paste0("https://info.geoconnex.us/collections/counties/items?STATEFP=",stateFips))
mapview::mapview(county)
  
#create name - remove anything before "County"... or before ","
county <- county %>% rename(name = NAME, GEOID = id) %>% select(GEOID, name)
county <- county %>% ms_simplify(keep=0.5, keep_shapes=TRUE)
geojson_write(county, file = paste0(swd_html, "county.geojson"))
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = county,  fillOpacity= 0.6,  fillColor = "gray", color="black",  weight=3)

#read in huc8
huc8 <- read_sf(paste0("https://info.geoconnex.us/collections/hu08/items?bbox=", paste(sf::st_bbox(state), collapse = ","))); #includes hucs outside of NC
#intersect with state to keep those
huc8.keep <- st_intersection(huc8, state)
huc8 <- huc8 %>% filter(id %in% huc8.keep$id)
#leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = huc8,  fillOpacity= 0.6,  fillColor = "gray", color="black",  weight=3)
huc8 <- huc8 %>% rename(huc8 = id, name = NAME) %>% select(huc8, name, uri, geometry) %>% ms_simplify(keep=0.5, keep_shapes=TRUE)
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = huc8,  fillOpacity= 0.6,  fillColor = "gray", color="black",  weight=1); #better check than mapview to ensure correct projections
geojson_write(huc8, file = paste0(swd_html, "huc8.geojson"))


huc6 <- read_sf(paste0("https://info.geoconnex.us/collections/hu06/items?bbox=", paste(sf::st_bbox(state), collapse = ","))); #includes hucs outside of NC
#intersect with state to keep those
huc6.keep <- st_intersection(huc6, state)
huc6 <- huc6 %>% filter(id %in% huc6.keep$id)
huc6 <- huc6 %>% rename(huc6 = id, name = NAME) %>% select(huc6, name, uri, geometry) %>% ms_simplify(keep=0.5, keep_shapes=TRUE)
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = huc6,  fillOpacity= 0.6,  fillColor = "gray", color="black",  weight=1); #better check than mapview to ensure correct projections
geojson_write(huc6, file = paste0(swd_html, "huc6.geojson"))


######################################################################################################################################################################
#
#   Create Rivers and Water Supply Watersheds for North Carolina
#
######################################################################################################################################################################
#Read in major streams from NC DEQ API
rivers <- read_sf("https://opendata.arcgis.com/datasets/2a5324f732bb44d9a1ca29f1c17370ee_0.geojson")
rivers <- st_transform(rivers, CRS("+init=epsg:4326")) %>% select(StrmName, geometry)
rivers <- ms_simplify(rivers, keep = 0.1, keep_shapes=TRUE); #can decide how much to simplify... has big impact on file size
geojson_write(rivers, file =  paste0(swd_html, "rivers.geojson"))


#water supply watersheds
ws <- read_sf("https://opendata.arcgis.com/datasets/fb32d3871a5640a986b72087c4121125_0.geojson") %>% st_transform(ws, crs="+init=epsg:4326")
#merge together polygons and count how many watersheds are included
ws <- ws %>%  ms_simplify(keep=0.08, keep_shapes=TRUE) %>% select(STREAM_NAM, geometry) %>% group_by(STREAM_NAM) %>% summarize(nSheds = n(), .groups="drop") %>% mutate(drawFile = "none")

#link to pwsid based on a manually created spreadsheet
link.df <- read.csv(paste0(swd_html, "link_pwsid_watershed.csv")) %>% select(pwsid, utility_name, ws_watershed)
link.ws <- merge(ws, link.df[,c("pwsid", "ws_watershed")], by.x="STREAM_NAM", by.y="ws_watershed") %>% mutate(drawFile = pwsid) %>% select(-pwsid) %>% group_by(STREAM_NAM, nSheds, drawFile, geometry) %>% distinct()
ws <- rbind(ws, link.ws)
#note that this creates duplicates of watersheds shared by utilities. This is necessary in order to draw individual watersheds based on utility selection. There may be better ways to do this. Just my solution.
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = ws,  fillOpacity= 0.4,  fillColor = "blue", color="black",  weight=1); #better check than mapview to ensure correct projections
geojson_write(ws, file=paste0(swd_html, "water_supply_watersheds.geojson"))




