###
# httr
# parsedate
#jsonlite
creds <- Sys.getenv(c("STA_ENDPOINT","STA_USER","STA_PW"))
endpoint <- creds["STA_ENDPOINT"]
user <- creds["STA_USER"]
pw <- creds["STA_PW"]

setwd("/src")
PostTestObs <- function(api, user, password,
                    result,
                    resultTime){
  o <- jsonlite::toJSON(list(
    result = 100,
    resultTime = resultTime,
    phenomenonTime = resultTime
  ), auto_unbox=TRUE)
  
  httr::POST(url = paste0(api,"Observations"), encode="json", httr::authenticate(user, password, type ="basic"),
       body = o)
  
  #return(o)
}

PostTestObs(endpoint, 
            user=user,
            password=pw,
            result=runif(1),
            resultTime=parsedate::format_iso_8601(Sys.time()) )  

x <- data.frame(c(endpoint, as.character(Sys.time())))
write.csv(x,"x.csv")
  
