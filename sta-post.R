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
                                 name,
                                 definition,
                                 description){
  
  op <- jsonlite::toJSON(list(name = name,
                              definition = definition,
                              description = description), auto_unbox=TRUE)
  
  POST(url = api, encode="json", authenticate(user, password, type ="basic"),
       body = op)
  
}

PostSensor <- function(api, user, password,
                       name,
                       description,
                       encodingType,
                       metadata){
  
  s <- jsonlite::toJSON(list(name = name,
                             description = description,
                             encodingType = "application/pdf",
                             metadata = metadata))
  POST(url = api, encode="json", authenticate(user, password, type ="basic"),
       body = s)
}