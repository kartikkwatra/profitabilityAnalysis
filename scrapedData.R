library(tidyverse)
library(rvest)
library(lubridate)


genericpath <- "/media/kartik/disk2/Data/DataStories/Agri data - HIL/Analysis/data/"

combine_data <- function(cropList,yearList,monthList) {
  cropDf <- data.frame()
  for(crop in cropList) {
    for(year in yearList ) {
      for(month in monthList) {
        # path construction
        #year <- as.character(year)
        filename <- "Agmarknet_State_wise_Wholesale_Prices_Monthly_Analysis.xls"
        path <- paste0(genericpath,crop,"/",year,"/",month,"/",filename)
        monthdata <- path %>% read_html() %>% html_table()
        monthdata <- monthdata[[1]]
        monthdata <- monthdata[1:(length(monthdata)-4) ]
        monthdata <- monthdata %>% mutate(month = month, year = year, date = as.Date(paste0(month,"/15/", year) ,format='%B/%d/%Y') )
        monthdata <- monthdata %>% mutate( fiscal = ifelse(month(date) <= 3, as.numeric(year)-1, as.numeric(year)))
        
        cropDF <- rbind(cropDF,monthdata)
      }
    }
  } 
  return(cropDF)
}

clean_data <- function(cropDF) {
  return(cropDF %>% filter(X1 != "State", X1 != "Average", !is.na(X2), X2 != "" )) 
}

filter_by_harvest <- function(crop, cropDF) {
  dictionary <- monthMatrix %>% filter(Crop == crop) %>% select(- Crop)
  return(semi_join(cropDF,dictionary)) 
}


# Groundnut
groundnut <- data.frame()
cropName <- c("Groundnut pods (raw)")
monthList <- c('September', 'October', 'November', 'December')
yearList <- as.character(seq(2006,2017, by=1))

combine_data(cropName,yearList,monthList)
groundnut <- clean_data(groundnut)
groundnut <- groundnut %>% rename(State = X1,Price = X2, Month = month) 

testDF <- filter_by_harvest(cropName,groundnut)


groundnut$X2 <- as.numeric(groundnut$X2)
wsp <- groundnut %>% group_by(fiscal,X1) %>% filter(!is.na(X2)) %>% filter( any(n() >= 3)) %>%
  summarise(avgPrice = mean(X2,na.rm = TRUE))
wsp <- rename(wsp, State = X1 )
wsp$fiscal <- as.character(wsp$fiscal)




gn <- inner_join(groundnutCost,wsp, by = c("State" = "State"  , "Year" = "fiscal" ) )

gn <- gn %>% mutate(diffPrice = FHP - avgPrice)

gn %>% ggplot() +
  geom_bar( aes(State,diffPrice, fill = Year) , stat = "identity", position = "dodge")


#wsp %>%
#  ggplot() +
#  geom_bar(aes( X1, avgPrice , fill = as.factor(fiscal) ), stat = "identity", position = "dodge" )



