# maize Everything
library(tidyverse)
library(rvest)
library(lubridate)

# Get the data from file
maizeCost <- readxl::read_xlsx("kharif.xlsx",sheet = "maize" )
unique(maizeCost$State)

# Getting the prices from file
maizePrices <- data.frame()
cropName <- c("Maize")
monthList <- c('September', 'October', 'November', 'December' )
yearList <- as.character(seq(2006,2014, by=1))

maizePrices <- combine_data(cropName,yearList,monthList,maizePrices,fmonth = 5)
maizePrices <- clean_data(maizePrices)
maizePrices <- maizePrices %>% rename(State = X1,Price = X2, Month = month) 

# No of months of Harvest for each crop
monthMatrix <- readxl::read_xlsx("matrix.xlsx")
harvestSummary <- monthMatrix %>% group_by(Crop,State) %>% summarise(num = n())

# Getting list of states which have atleast x price points per and with available costs
maizePricesSubsetLarge <- maizePrices %>%
  group_by(fiscal,State) %>%
  filter( any( 
    n() >=
      case_when(
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 1 ~ 1,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 2 ~ 1,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 3 ~ 2,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 4 ~ 2,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 5 ~ 3,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 6 ~ 4,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 7 ~ 5,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 8 ~ 5
        #TRUE ~ 100
      )
  )
  )

# Checking for Min Max - Ideally should happen earlier: Just after creating the subset
str(maizePricesSubsetLarge)
maizePricesSubsetLarge$Price <- as.numeric(maizePricesSubsetLarge$Price)
temp <- maizePricesSubsetLarge %>% group_by(State, fiscal) %>% 
  summarise(min = min(Price), max = max(Price))


#maizeWSP <- maizePricesSubset %>% group_by(fiscal,State)
#temp <- semi_join(maize,maizeCost, by = c("State" = "State", "fiscal" = "Year"))
#unique(temp$State)

# Now Cleaning for harvest
maizePricesSubsetSmall <-  filter_by_harvest(cropName,maizePricesSubsetLarge)

# Imp to check or filter only those groups where the minimum number of data points is satisfied
maizePricesSubsetSmaller <- maizePricesSubsetSmall %>%
  group_by(fiscal,State) %>%
  filter( any( 
    n() >=
      case_when(
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 1 ~ 1,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 2 ~ 1,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 3 ~ 2,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 4 ~ 2,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 5 ~ 3,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 6 ~ 4
      )
  )
  )

# WSP for all 
# already grouped by fiscal year and state
maizePricesSubsetSmaller$Price <- as.numeric(maizePricesSubsetSmaller$Price)
maizePricesSubsetSmaller$fiscal <- as.integer(maizePricesSubsetSmaller$fiscal)
maizeWSPSmaller <- maizePricesSubsetSmaller %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

maizePricesSubsetLarge$Price <- as.numeric(maizePricesSubsetLarge$Price)
maizePricesSubsetLarge$fiscal <- as.integer(maizePricesSubsetLarge$fiscal)
maizeWSPLarge <- maizePricesSubsetLarge %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

# Not joining
temp <- anti_join(maizeWSPLarge,maizeWSPSmaller, by= c("State","fiscal"))
maizeWSP <- maizeWSPSmaller


# Now joining the Cost and WSP tables: Our Final Table
maize <- inner_join(maizeCost,maizeWSP, by = c("State" = "State",  "Year" = "fiscal" ))


###### ANALYSIS ########

# be careful of periods: should be comparable for both  
# Average annual growth rates of C2
#maizeCost$Year <- as.numeric(maizeCost$Year)
#maizeCost <- maizeCost %>% group_by(State) %>%
#  arrange(State,Year) %>%
#  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

## net margins

maize <- maize %>% filter( !(State %in% c("Bihar","Maharashtra","Punjab")) ) %>%
  mutate( margin = avgWSP - C2, marginPercent = 100*margin/C2)  

mz <- maize %>%
  ggplot() +
  geom_bar( aes(x = State, y = marginPercent,
                fill = factor(Year),  alpha = factor(Year),
                width = 0.6 ),
            stat = "identity", position = "dodge") +
  labs(x = "States", y = "Percentage Net Return in Rs/Quintal",
       title = "Net Returns in Maize growing states",
       fill = "Year", alpha = "Year")

ggplotly(mz)

# Average annual growth rates of avgWSP and C2 for the same period
maize <- maize %>% group_by(State) %>%
  arrange(State,Year) %>%
  mutate(WSPGrowth = 100*((avgWSP - lag(avgWSP,1))/lag(avgWSP,1))/(Year - lag(Year,1) )) %>% 
  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

maize %>% group_by(State) %>%
  summarise(C2AAGR = mean(C2Growth, na.rm = TRUE), WSPAAGR = mean(WSPGrowth, na.rm = TRUE), avgAnnualProfitMarginPercent =  mean(marginPercent, na.rm = TRUE) )

