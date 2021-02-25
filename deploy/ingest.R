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



readTemplate <- function(path){
  httr::GET("https://durhamcity-my.sharepoint.com/:x:/g/personal/james_lim_durhamnc_gov/EcCSiOcOVjBGvfne8FrFuE8BpTPH9uCVa-6WNM0sQUmqiw?download=1", 
            httr::write_disk("test2.xlsx", overwrite=TRUE)
  meta <- readxl::read_excel("/tmp/tmp.xlsx", sheet="system_metadata")
  return(meta)
}

https://durhamcity-my.sharepoint.com/:x:/g/personal/james_lim_durhamnc_gov/EcCSiOcOVjBGvfne8FrFuE8BpTPH9uCVa-6WNM0sQUmqiw?download=1