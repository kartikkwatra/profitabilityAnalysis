# Cotton Everything
library(tidyverse)
library(rvest)
library(lubridate)

# Get the data from file
cottonCost <- readxl::read_xlsx("kharif.xlsx",sheet = "cotton" )

# Fiscal year set.
#cottonCost <- cottonCost %>% mutate(Year = substr(Year,1,4) )

# Removing Orissa
#cottonCost <- cottonCost %>% filter(State != "Orissa", State != "Orrisa"  )

# Getting the prices from file
cottonPrices <- data.frame()
cropName <- c("Cotton")
monthList <- c('October', 'November', 'December', 'January','February','March','April','May' )
yearList <- as.character(seq(2006,2017, by=1))

cottonPrices <- combine_data(cropName,yearList,monthList,cottonPrices,fmonth = 5)
cottonPrices <- clean_data(cottonPrices)
cottonPrices <- cottonPrices %>% rename(State = X1,Price = X2, Month = month) 

# No of months of Harvest for each crop
monthMatrix <- readxl::read_xlsx("matrix.xlsx")
harvestSummary <- monthMatrix %>% group_by(Crop,State) %>% summarise(num = n())

# Getting list of states which have atleast x price points per and with available costs
cottonPricesSubsetLarge <- cottonPrices %>%
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
str(cottonPricesSubsetLarge)
cottonPricesSubsetLarge$Price <- as.numeric(cottonPricesSubsetLarge$Price)
temp <- cottonPricesSubsetLarge %>% group_by(State, fiscal) %>% 
  summarise(min = min(Price), max = max(Price))


#cottonWSP <- cottonPricesSubset %>% group_by(fiscal,State)
#temp <- semi_join(cotton,cottonCost, by = c("State" = "State", "fiscal" = "Year"))
#unique(temp$State)

# Now Cleaning for harvest
cottonPricesSubsetSmall <-  filter_by_harvest(cropName,cottonPricesSubsetLarge)

# Imp to check or filter only those groups where the minimum number of data points is satisfied
cottonPricesSubsetSmaller <- cottonPricesSubsetSmall %>%
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
cottonPricesSubsetSmaller$Price <- as.numeric(cottonPricesSubsetSmaller$Price)
cottonPricesSubsetSmaller$fiscal <- as.integer(cottonPricesSubsetSmaller$fiscal)
cottonWSPSmaller <- cottonPricesSubsetSmaller %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

cottonPricesSubsetLarge$Price <- as.numeric(cottonPricesSubsetLarge$Price)
cottonPricesSubsetLarge$fiscal <- as.integer(cottonPricesSubsetLarge$fiscal)
cottonWSPLarge <- cottonPricesSubsetLarge %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

# Found a difference in the number of cases b/w cottonWSPSmaller and Large, so joining
temp <- anti_join(cottonWSPLarge,cottonWSPSmaller, by= c("State","fiscal"))
cottonWSP <- rbind(cottonWSPSmaller,temp)


# Now joining the Cost and WSP tables: Our Final Table
cotton <- inner_join(cottonCost,cottonWSP, by = c("State" = "State",  "Year" = "fiscal" ))


###### ANALYSIS ########

# be careful of periods: should be comparable for both  
# Average annual growth rates of C2
#cottonCost$Year <- as.numeric(cottonCost$Year)
#cottonCost <- cottonCost %>% group_by(State) %>%
#  arrange(State,Year) %>%
#  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

## net margins

cotton <- cotton %>%
  mutate( margin = avgWSP - C2, marginPercent = 100*margin/C2)  

cotton %>%
  ggplot() +
  geom_bar( aes(x = State, y = marginPercent, fill = factor(Year) ) , stat = "identity", position = "dodge")

# Average annual growth rates of avgWSP and C2 for the same period
cotton <- cotton %>% group_by(State) %>%
  arrange(State,Year) %>%
  mutate(WSPGrowth = 100*((avgWSP - lag(avgWSP,1))/lag(avgWSP,1))/(Year - lag(Year,1) )) %>% 
  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

cotton %>% group_by(State) %>%
  summarise(C2AAGR = mean(C2Growth, na.rm = TRUE), WSPAAGR = mean(WSPGrowth, na.rm = TRUE), avgAnnualProfitMarginPercent =  mean(marginPercent, na.rm = TRUE) )

# Removing the 2010 outlier
cotton %>% filter(Year != "2010") %>%  group_by(State) %>%
  arrange(State,Year) %>%
  mutate(WSPGrowth = 100*((avgWSP - lag(avgWSP,1))/lag(avgWSP,1))/(Year - lag(Year,1) )) %>% 
  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) )) %>% 
  summarise(C2AAGR = mean(C2Growth, na.rm = TRUE), WSPAAGR = mean(WSPGrowth, na.rm = TRUE), avgAnnualProfitMarginPercent =  mean(marginPercent, na.rm = TRUE) )
