# Initialize all triangle CWS locations

## all CWS boundaries
#b <- sf::read_sf("https://aboutus.internetofwater.dev/geoserver/ows?service=WFS&version=1.0.0&request=GetFeature&typename=geonode%3Anc_statewide_CWS&outputFormat=json&srs=EPSG%3A2264&srsName=EPSG%3A2264")
#sf::st_write(b,"data/boundaries.gpkg")


#change endpoint to environmental variable
endpoint <- "http://localhost:8080/FROST-Server/v1.1/"

b <- sf::read_sf("data/boundaries.gpkg")
b <- sf::st_transform(b,4326)
user = "iow"
password = "nieps"

staPost <- function(url, payload, user, password) {
  httr::POST(
    url = url,
    encode = "json",
    httr::authenticate(user, password, type = "basic"),
    body = payload
  )
}

formatLoc <- function(b, index){
  id <- paste0("NC",gsub("-","",b$PWSID[index]))
  name <- b$SystemName[index]
  y <- geojsonsf::sfc_geojson(b$geom[index])
  
  loc <- list(
    `@iot.id` = paste0(id),
    name = paste0(id, " Service area"),
    description = paste0("Service area of ",name),
    encodingType = "application/vnd.geo+json",
    location = fromJSON(y)
  )
  
  return(loc)
}


for (i in 1:length(b$PWSID)){
  
  loc <- formatLoc(b,i)
  
  staPost(
    url = paste0(endpoint, "Locations"),
    payload = loc,
    user = "iow",
    password = "nieps"
  )
  print(paste0(loc$name, " is done, ", i," of ",length(b$PWSID)))
}
