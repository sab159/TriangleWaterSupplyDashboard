### httr parsedate jsonlite
endpoint <- "http://web:8080/FROST-Server/v1.1/"
user <- "iow"
pw <- "nieps"

setwd("/src")

PostThing <- function(api, user, password, name, description) {
  t <-
    jsonlite::toJSON(list(name = name, description = description,), auto_unbox = TRUE)
  
  httr::POST(
    url = paste0(api, "Things"),
    encode = "json",
    httr::authenticate(user, password, type = "basic"),
    body = t
  )
}

PostThing(
  endpoint,
  user = user,
  password = pw,
  name = "Test Thing",
  description = as.character(parsedate::format_iso_8601(Sys.time()))
)

x <- data.frame(c(endpoint, as.character(Sys.time())))
write.csv(x, "x.csv")


path <-
  "https://durhamcity-my.sharepoint.com/:x:/g/personal/james_lim_durhamnc_gov/EcCSiOcOVjBGvfne8FrFuE8BpTPH9uCVa-6WNM0sQUmqiw?download=1"

readTemplate <- function(path) {
  httr::GET(path, httr::write_disk("tmp/tmp.xlsx", overwrite = TRUE))
  meta <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "system_metadata") %>% tibble::column_to_rownames(var = "field") %>% t() %>% as_tibble()
  
  sources <- readxl::read_excel("tmp/tmp.xlsx", sheet = "sources")
  monitoring_locations <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "monitoring_locations")
  conservation_policies <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "conservation_policies")
  delivery <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "delivery")
  supply <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "supply_conditions")
  monitoring_data <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "monitoring_data")
  conservation_status <-
    readxl::read_excel("tmp/tmp.xlsx", sheet = "conservation_status")
  
  unlink("tmp/tmp.xlsx")
  list <-
    list(
      metadata = meta,
      sources = sources,
      monitoring_locations = monitoring_locations,
      conservation_policies = conservation_policies,
      delivery = delivery,
      supply = supply,
      monitoring_data = monitoring_data,
      conservation_status = conservation_status
    )
  
  return(list)
}

metaToUtilityThing <- function(meta) {
  
  id <- paste0("NC", gsub("-", "", meta$pwsid))
  service_area <- sf::read_sf(paste0("https://geoconnex.us/ref/pws/",id))$geometry
  service_sfc <- sf::st_sfc(service_area$geometry)
  
  thing <-
      list(
        `@iot.id` = id,
        name = meta$system_name,
        description = paste0("Data from the water utility: ",
                             meta$system_name),
        properties = list(
          county = meta$county,
          basin = meta$basin,
          ownership = meta$ownership,
          service_population = meta$service_population,
          contact_name = paste0(meta$contact_first_name, " ", meta$contact_last_name),
          contact_title = meta$contact_title,
          address = meta$address,
          city = meta$city,
          state = meta$state,
          zip = meta$zip,
          phone = meta$phone,
          fax = meta$fax,
          email = meta$email,
          wsrp_link = meta$wsrp_link
        )
        )
  
  loc <- list(
          `@iot.id` = paste0(id," - Service Area"),
          name = paste0(id, " Service area"),
          description = paste0("Service area of ",thing$name),
          encodingType = "application/vnd.geo+json",
          location = list(
            type="Point",
            coordinates = list(
              4.9,
              52.3
            )
          )
        )
      
  utility <- list(thing=thing,location=loc)    
    
  return(utility)
}

staPost <- function(url, payload, user, password) {
  httr::POST(
    url = url,
    encode = "json",
    httr::authenticate(user, password, type = "basic"),
    body = payload
  )
}

staPatch <- function(url, payload, user, password) {
  httr::PATCH(
    url = url,
    encode = "json",
    httr::authenticate(user, password, type = "basic"),
    body = payload
  )
}

strip_iot_id <- function(list){
  list$`@iot.id` <- NULL
  return(list)
}

uploadUtility <- function(api, utility, user, password) { 
  id <- utility$thing$`@iot.id`
  id.l <- utility$location$`@iot.id`
  
  status <- httr::GET(paste0(api, "Things('", id, "')"))$status
  if (status == 200) {
    
    staPatch(
      url = paste0(api, "Things('", id, "')"),
      payload = strip_iot_id(utility$thing),
      user = user,
      password = password
    )
    
    staPatch(
      url = paste0(api, "Locations('", id.l, "')"),
      payload = strip_iot_id(utility$location),
      user = user,
      password = password
    )
  } else {
    staPost(
      url = paste0(api, "Things"),
      payload = utility$thing,
      user = user,
      password = password
    )
    staPost(
      url = paste0(api, "Things('", id, "')/Locations"),
      payload = utility$location,
      user = user,
      password = password
    )
  }
}
