PostThingLocation <- function(api, user, password,
                              ThingName,
                              ThingDescription,
                              ThingPropertyList,
                              LocationName,
                              LocationDescription,
                              LocationGeometry) {
  thing <- jsonlite::toJSON(list(name = "OWASA",
                                     description = "Orange Water and Sewer Authority",
                                     properties = list(PWSID="03-68-010")), auto_unbox=TRUE)
  
  
  
  json_body2 <- jsonlite::toJSON(list(name = "OWASA Service Area",
                                      description = "Orange Water and Sewer Authority Service Area",
                                      encodingType = "application/vnd.geo+json",
                                      location = ), auto_unbox = TRUE) 
  

  POST(url = api, encode="json", authenticate(user, password, type ="basic"),
             body = list(name = "OWASA",
                         description = "Orange Water and Sewer Authority",
                         properties = list(PWSID="03-68-010"))
             )
}

PostObservedProperty <- function(api, user, password,
                                 id,
                                 name,
                                 definition,
                                 description){
  
  op <- jsonlite::toJSON(list(`@iot.id`= id,
                              name = name,
                              definition = definition,
                              description = description), auto_unbox=TRUE)
  
  POST(url = api, encode="json", authenticate(user, password, type ="basic"),
       body = op)
  
}

PostDataStream <- function(api, user, password,
                           id,
                           name,
                           description,
                           observationType= c("http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement",
                                              "http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_CategoryObservation"),
                           unit,
                           Thing_id,
                           ObservedProperty_id,
                           Sensor_id){
  ds <- jsonlite::toJSON(list(`@iot.id`= id,
                              name = name,
                              description = description,
                              observationType = observationType,
                              unitOfMeasurement = unit,
                              Thing = list(`@iot.id` = Thing_id),
                              ObservedProperty=list(`@iot.id`=ObservedProperty_id),
                              Sensor = list(`@iot.id`=Sensor_id)
                              ), auto_unbox=TRUE)
  
  POST(url = api, encode="json", authenticate(user, password, type ="basic"),
       body = ds)
  
  #return(ds)
}

PostSensor <- function(api, user, password,
                       id,
                       name,
                       description,
                       encodingType,
                       metadata){
  
  s <- jsonlite::toJSON(list(`@iot.id` = id,
                             name = name,
                             description = description,
                             encodingType = "application/pdf",
                             metadata = metadata), auto_unbox=TRUE)
  POST(url = api, encode="json", authenticate(user, password, type ="basic"),
       body = s)
}

PostObs <- function(api, user, password,
                    ds,
                    result,
                    resultTime,
                    days){
  o <- jsonlite::toJSON(list(
    `@iot.id` = paste0(ds,"-",resultTime),
    result = result,
    resultTime = resultTime,
    phenomenonTime = resultTime,
    parameters = list(
      `days in observed period` = days
    ),
    Datastream = list(`@iot.id` = ds)
  ), auto_unbox=TRUE)
  
  POST(url = paste0(api,"Observations"), encode="json", authenticate(user, password, type ="basic"),
       body = o)
  
  #return(o)
}