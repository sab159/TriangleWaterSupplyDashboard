###

endpoint <- "http://web:8080/FROST-Server/v1.1/"
user <- "iow"
pw <- "nieps"

setwd("/src")

PostThing <- function(api, user, password, name, description){
	t <- jsonlite::toJSON(list(
		name = name,
		description = description), auto_unbox=TRUE)
		
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

#run script with global endpoints
source("*global0_set_apis_libraries.R")
source("*use1_streamflow_data.R")

