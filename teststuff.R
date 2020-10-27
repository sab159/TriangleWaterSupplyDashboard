
# Define a Thing/ Location
x <- sf::read_sf("https://geoconnex.us/ref/pws/NC0368010")
x <- x$geometry
x <- sfc_geojson(sf::read_sf("https://geoconnex.us/ref/pws/NC0368010")$geometry)

thing <- jsonlite::toJSON(list(name = "OWASA",
                               description = "Orange Water and Sewer Authority",
                               properties = list(PWSID="03-68-010")), auto_unbox=TRUE)

p <- POST(url = "https://twsd.internetofwater.dev/api/v1.0/Things",
          encode="raw",
          authenticate("iow", "nieps", type ="basic"),
          body = thing)



loc <- jsonlite::toJSON(list(name = "OWASA Service Area",
                                    description = "Orange Water and Sewer Authority Service Area",
                                    encodingType = "application/vnd.geo+json",
                                    location = x), auto_unbox = TRUE) 

loc <- paste0('{"name":"OWASA Service Area","description":"Orange Water and Sewer Authority Service Area","encodingType":"application/vnd.geo+json","location":',
              x,'}')

loc <- toJSON(loc,auto_unbox = TRUE) 
loc <- fromJSON(loc)

p2 <- POST(url = paste0(p$headers$location,"/Locations"),
          encode="raw",
          authenticate("iow", "nieps", type ="basic"),
          body = loc)


o <- 

POST(url = "https://twsd.internetofwater.dev/api/v1.0/Things", encode="json", authenticate("iow", "nieps", type ="basic"),
body = list(name = "OWASA",
description = "Orange Water and Sewer Authority",
properties = list(PWSID="03-68-010")))

deleteEntity<- function(api=api,user=user,password=password,entity= c("Things","Locations"),iot.id){
  DELETE(url = paste0(api,entity,"(",iot.id,")"), authenticate("iow", "nieps", type ="basic"))
}

api <- "https://twsd.internetofwater.dev/api/v1.0/"
user <- "iow"
password <- "nieps"

deleteEntity(api,user,password,entity="Things",iot.id=5)

body <- paste0(json_body1,",",loc)

body = ""{
  "name": "Netatmo 1",
  "description": "Netatmo VZ",
  "properties": {
    "organisation": "Geodan",
    "owner": "Tim"
  },
  "Locations": [
    {
      "name": "President Kennedylaan 1",
      "description": "Geodan building Amsterdam",
      "encodingType": "application/vnd.geo+json",
      "location": {
        "type": "Point",
        "coordinates": [
          4.9132,
          52.34227
          ]
      }
    }
    ]
}"
  