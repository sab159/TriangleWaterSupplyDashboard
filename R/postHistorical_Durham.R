########################
# This file uploads the historical demand data from Durham to SensorThings


api <- "https://twsd.internetofwater.dev/api/v1.1/"
user <- "iow"
password <- "nieps"

source("R/libraries.R")
source("R/sta-post.R")

#### Post Data Stream
responseDS <- PostDataStream(api=paste0(api,"Datastreams"),
                             user=user,
                             password=password,
                             id="NC0332010-WaterDistributed",
                             name="Water distributed (MGD) by Durham_City_Of",
                             description = "Total Finished Water + Purchases - Sales (MG) in day",
                             observationType = "http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement",
                             unit = list(definition="http://his.cuahsi.org/mastercvreg/edit_cv11.aspx?tbl=Units&id=1125579048",
                                         name = "Million Gallons per Day",
                                         symbol = "MGD"),
                             Thing_id = "NC0332010",
                             ObservedProperty_id = "WaterDistributed",
                             Sensor_id = "DemandReport")


#### Read excel file

d <- readxl::read_excel("data/Historical/FinishedWaterDemand_Durham_012121.xlsx",
                        sheet="finished water daily demand", skip=3)

#### Transform long

d <- d[1:365,2:25]
d$Month <- as.numeric(d$Month)
d$Month <- paste0("0",d$Month)
d$Month[which(d$Month=="010")] <- "10"
d$Month[which(d$Month=="011")] <- "11"
d$Month[which(d$Month=="012")] <- "12"

d$Day[which(d$Day<10)] <- paste0("0",d$Day[which(d$Day<10)])

l <- d %>% 
  pivot_longer(cols = !Month & !Day,
               names_to = "Year",
               values_to = "result") %>%
  arrange(Year,Month,Day) %>%
  filter(!is.na(result))


#### Format data ISO8061
l$resultTime <- paste0(l$Year,"-",
                       l$Month,"-",
                       l$Day,"T",
                       "00:00:00.000Z")

for(i in 1:length(l$result)){
  try(
    PostObs(api,user, password,
            ds = "NC0332010-WaterDistributed",
            result = l$result[i],
            resultTime = l$resultTime[i],
            days = 1)
    
  )
  print(paste0("Posted ",i," in ",length(l$result)))
}
