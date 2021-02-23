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
pwsid.list <- read.csv(paste0(swd_html, "basic_info.csv"), header=TRUE)


######################################################################################################################################################################
#
#   Create Utility Map Layer
#
######################################################################################################################################################################
#read in water systems - note that this link may change over time
baseURL =  "https://aboutus.internetofwater.dev/geoserver/ows?service=WFS&version=1.0.0&request=GetFeature&typename=geonode%3APWS_NC_20190&outputFormat=json&srs=EPSG%3A2264&srsName=EPSG%3A2264"
  nc.systems <- read_sf(baseURL)
  
  #limit to only those in triangle study
  nc.systems <- nc.systems %>% st_transform(crs = 4326) %>% select(PWSID) %>% mutate(ncpwsid = paste0("NC", str_remove_all(PWSID, "[-]"))) %>% mutate(ncpwsid = str_remove_all(ncpwsid, "[_]")); #one of the pwsid has a data entry error
  nc.systems <- merge(nc.systems, pwsid.list, by.x="ncpwsid", by.y="pwsid"); #merge to get preferred utility names into the shapefile
  nc.systems <- nc.systems %>% select(PWSID, ncpwsid, utility_name, data)

  #simplify and reduce size
  nc.systems <- ms_simplify(nc.systems, keep = 0.08, keep_shapes=TRUE)
  mapview::mapview(nc.systems)
  geojson_write(nc.systems, file =  paste0(swd_html, "nc_utilities.geojson"))

  
######################################################################################################################################################################
#
#   Create River Basin and Watershed Layers
#
######################################################################################################################################################################
#read in HUC8 shapefile
outdir = swd_html; #set to a download folder
download_wbd(outdir, url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/", "Hydrography/WBD/National/GDB/WBD_National_GDB.zip"))  

#read in geodatabase
gdb <- path.expand("C:\\Users\\lap19\\Documents\\GIS\\HUCs\\WBD_National_GDB.gdb")    
ogrListLayers(gdb)
huc8 <- readOGR(gdb, "WBDHU8")

#keep HUC8 for NC
nc <- huc8[str_detect(huc8$states,"NC")==TRUE,]; #filter to nc
#simplify and transform and safe out
nc <- nc %>% st_as_sf() %>% st_transform(crs = 4326) %>% ms_simplify(keep=0.05, keep_shapes=TRUE) %>% select(name, areasqkm, huc8)
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = nc,  fillOpacity= 0.6,  fillColor = "gray", color="black",  weight=1); #better check than mapview to ensure correct projections
geojson_write(nc, file = paste0(swd_html, "huc8.geojson"))

#Keep Huc 6 for NC
huc6 <- readOGR(gdb, "WBDHU6")
nc <- huc6[str_detect(huc6$states,"NC")==TRUE,]; #filter to nc
#simplify and transform and safe out
nc <- nc %>% st_as_sf() %>% st_transform(crs = 4326) %>% ms_simplify(keep=0.05, keep_shapes=TRUE) %>% select(name, areasqkm, huc6)
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = nc,  fillOpacity= 0.6,  fillColor = "gray", color="black",  weight=1); #better check than mapview to ensure correct projections
geojson_write(nc, file = paste0(swd_html, "huc6.geojson"))


######################################################################################################################################################################
#
#   Create Rivers and Water Supply Watersheds for North Carolina
#
######################################################################################################################################################################
#Read in major streams from NC DEQ API
rivers <- read_sf("https://opendata.arcgis.com/datasets/2a5324f732bb44d9a1ca29f1c17370ee_0.geojson")
rivers <- st_transform(rivers, CRS("+init=epsg:4326"))
rivers <- ms_simplify(rivers, keep = 0.1, keep_shapes=TRUE); #can decide how much to simplify... has big impact on file size
geojson_write(rivers, file =  paste0(swd_html, "rivers.geojson"))


#water supply watersheds
ws <- read_sf("https://opendata.arcgis.com/datasets/fb32d3871a5640a986b72087c4121125_0.geojson") %>% st_transform(ws, crs="+init=epsg:4326")
#merge together polygons and count how many watersheds are included
ws <- ws %>%  ms_simplify(keep=0.08, keep_shapes=TRUE) %>% select(STREAM_NAM, geometry) %>% group_by(STREAM_NAM) %>% summarize(nSheds = n(), .groups="drop") %>% mutate(drawFile = "none")
#link to pwsid based on a manually created spreadsheet
link.df <- read.csv("M:\\public_html\\www\\nc-water-supply\\data\\link_pwsid_watershed.csv")
link.ws <- merge(ws, link.df[,c("pwsid", "ws_watershed")], by.x="STREAM_NAM", by.y="ws_watershed") %>% mutate(drawFile = pwsid) %>% select(-pwsid) %>% group_by(STREAM_NAM, nSheds, drawFile, geometry) %>% distinct()
ws <- rbind(ws, link.ws)
#note that this creates duplicates of watersheds shared by utilities. This is necessary in order to draw individual watersheds based on utility selection. There may be better ways to do this. Just my solution.
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = ws,  fillOpacity= 0.4,  fillColor = "blue", color="black",  weight=1); #better check than mapview to ensure correct projections
geojson_write(ws, file=paste0(swd_html, "water_supply_watersheds.geojson"))


######################################################################################################################################################################
#
#   CREATE COUNTY LAYER FROM CENSUS API
#
######################################################################################################################################################################
county <- get_acs(geography = "county", variables = "B01001_001E", state = "37", year = 2019, geometry = TRUE) %>% st_transform(crs = 4326); #pulls county variable
#create name - remove anything before "County"... or before ","
county <- county %>% mutate(name = gsub("(.*),.*", "\\1", NAME)) %>% mutate(name = substr(name,0,(nchar(name)-7))) %>% select(GEOID, name)
county <- county %>% ms_simplify(keep=0.45, keep_shapes=TRUE)
geojson_write(county, file = paste0(swd_html, "county.geojson"))
leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = county,  fillOpacity= 0.6,  fillColor = "gray", color="black",  weight=3)




  
  



