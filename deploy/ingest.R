###
# httr
# parsedate
#jsonlite
endpoint <- "http://web:8080/FROST-Server/v1.1/"
user <- "iow"
pw <- "nieps"

setwd("/src")

PostThing <- function(api, user, password, name, description){
	t <- jsonlite::toJSON(list(
		name = name,
		description = description,), auto_unbox=TRUE)
		
	httr::POST(url = paste0(api,"Things"), encode="json", httr::authenticate(user, password, type = "basic"),
			   body = t)
}

PostThing(endpoint, 
            user=user,
            password=pw,
            name="Test Thing",
            description=as.character(parsedate::format_iso_8601(Sys.time())) )  

x <- data.frame(c(endpoint, as.character(Sys.time())))
write.csv(x,"x.csv")


path <- "https://durhamcity-my.sharepoint.com/:x:/g/personal/james_lim_durhamnc_gov/EcCSiOcOVjBGvfne8FrFuE8BpTPH9uCVa-6WNM0sQUmqiw?download=1"

readTemplate <- function(path){
  httr::GET(path, httr::write_disk("tmp/tmp.xlsx", overwrite=TRUE))
  meta <- readxl::read_excel("tmp/tmp.xlsx", sheet="system_metadata") %>% 
    tibble::column_to_rownames(var="field") %>% 
    t() %>%
    as_tibble()
  
  sources <- readxl::read_excel("tmp/tmp.xlsx", sheet="sources")
  monitoring_locations <- readxl::read_excel("tmp/tmp.xlsx", sheet="monitoring_locations")
  conservation_policies <- readxl::read_excel("tmp/tmp.xlsx", sheet="conservation_policies")
  delivery <- readxl::read_excel("tmp/tmp.xlsx", sheet="delivery") 
  supply <- readxl::read_excel("tmp/tmp.xlsx", sheet="supply_conditions") 
  monitoring_data <- readxl::read_excel("tmp/tmp.xlsx", sheet="monitoring_data") 
  conservation_status <- readxl::read_excel("tmp/tmp.xlsx", sheet="conservation_status") 
  
  unlink("tmp/tmp.xlsx")
  list <- list("metadata"=meta,
               "sources"=sources,
               "monitoring_locations"= monitoring_locations,
               "conservation_policies"=conservation_policies,
               "delivery"=delivery,
               "supply"=supply,
               "monitoring_data"= monitoring_data,
               "conservation_status"= conservation_status)
  
  return(list)
}

metaToThing <- function(meta){
  t <- jsonlite::toJSON(list(
    name = name,
    description = description,), auto_unbox=TRUE)
}


https://durhamcity-my.sharepoint.com/:x:/g/personal/james_lim_durhamnc_gov/EcCSiOcOVjBGvfne8FrFuE8BpTPH9uCVa-6WNM0sQUmqiw?download=1