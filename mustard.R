# mustard Everything
library(tidyverse)
library(rvest)
library(lubridate)
library(plotly)

# Get the data from file
mustardCost <- readxl::read_xlsx("kharif.xlsx",sheet = "mustard" )

# Fiscal year set.
mustardCost <- mustardCost %>% mutate(Year = substr(Year,1,4) )
unique(mustardCost$State)

# Removing Orissa
#mustardCost <- mustardCost %>% filter(State != "Orissa", State != "Orrisa"  )

# Getting the prices from file
mustardPrices <- data.frame()
cropName <- c("Mustard")
monthList <- c('February','March','April') # Excluding May since no cost element with May in marketing months
yearList <- as.character(seq(2006,2017, by=1))

mustardPrices <- combine_data(cropName,yearList,monthList,mustardPrices, fmonth = 4)
mustardPrices <- clean_data(mustardPrices)
mustardPrices <- mustardPrices %>% rename(State = X1,Price = X2, Month = month)

# No of months of Harvest for each crop
monthMatrix <- readxl::read_xlsx("matrix.xlsx")
harvestSummary <- monthMatrix %>% group_by(Crop,State) %>% summarise(num = n())

# Getting list of states which have atleast x price points per and with available costs
mustardPricesSubsetLarge <- mustardPrices %>%
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

# Checking for Min Max - to find any odd
str(mustardPricesSubsetLarge)
mustardPricesSubsetLarge$Price <- as.numeric(mustardPricesSubsetLarge$Price)
temp <- mustardPricesSubsetLarge %>% group_by(State, fiscal) %>% 
  summarise(min = min(Price), max = max(Price))


#mustardWSP <- mustardPricesSubset %>% group_by(fiscal,State)
#temp <- semi_join(mustard,mustardCost, by = c("State" = "State", "fiscal" = "Year"))
#unique(temp$State)

# Now Cleaning for harvest
mustardPricesSubsetSmall <-  filter_by_harvest(cropName,mustardPricesSubsetLarge)

# Imp to check or filter only those groups where the minimum number of data points is satisfied
mustardPricesSubsetSmaller <- mustardPricesSubsetSmall %>%
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
mustardPricesSubsetSmaller$Price <- as.numeric(mustardPricesSubsetSmaller$Price)
mustardPricesSubsetSmaller$fiscal <- as.integer(mustardPricesSubsetSmaller$fiscal)
mustardWSPSmaller <- mustardPricesSubsetSmaller %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

mustardPricesSubsetLarge$Price <- as.numeric(mustardPricesSubsetLarge$Price)
mustardPricesSubsetLarge$fiscal <- as.integer(mustardPricesSubsetLarge$fiscal)
mustardWSPLarge <- mustardPricesSubsetLarge %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

# Found no difference in the number of cases b/w mustardWSPSmaller and Large, so Smaller
#temp <- anti_join(mustardWSPLarge,mustardWSPSmaller, by= c("State","fiscal"))
mustardWSP <- mustardWSPSmaller

mustardCost$Year <- as.numeric(mustardCost$Year)
# Now joining the Cost and WSP tables: Our Final Table
mustard <- inner_join(mustardCost,mustardWSP, by = c("State" = "State",  "Year" = "fiscal" ))


###### ANALYSIS ########

# be careful of periods: should be comparable for both  
# Average annual growth rates of C2
#mustardCost$Year <- as.numeric(mustardCost$Year)
#mustardCost <- mustardCost %>% group_by(State) %>%
#  arrange(State,Year) %>%
#  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

## net margins

mustard <- mustard %>%
  mutate( margin = avgWSP - C2, marginPercent = 100*margin/C2)  



# Average annual growth rates of avgWSP and C2 for the same period
mustard <- mustard %>% group_by(State) %>%
  arrange(State,Year) %>%
  mutate(WSPGrowth = 100*((avgWSP - lag(avgWSP,1))/lag(avgWSP,1))/(Year - lag(Year,1) )) %>% 
  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) )) %>%
  mutate(YieldGrowth = Yield - lag(Yield,1)) %>%
  mutate(ydGrowthPercent =  100*(YieldGrowth/lag(Yield,1))/(Year - lag(Year,1) )  )

summary<- mustard %>% group_by(State) %>%
  summarise(C2AAGR = mean(C2Growth, na.rm = TRUE),
            WSPAAGR = mean(WSPGrowth, na.rm = TRUE),
            avgAnnualYieldGrowth = mean(YieldGrowth, na.rm = TRUE),
            avgAnnualYieldGrowthPercent = mean(ydGrowthPercent, na.rm = TRUE),
            avgAnnualProfitMarginPercent = mean(marginPercent, na.rm = TRUE))

summary
gg<- mustard %>%
  ggplot() +
  geom_bar( aes(x = State, y = marginPercent, fill = factor(Year),
                alpha = factor(Year), width = 0.6) ,
            stat = "identity",
            position = "dodge") +
  labs(x = "States", y = "Percentage Net Return in Rs/Quintal",
       title = "Net Returns in Mustard growing states",
       fill = "Year", alpha = "Year")

gg
ggplotly(gg)

# checking from 2007 onwards
mustard %>% filter(Year > "2007") %>%  group_by(State) %>%
  arrange(State,Year) %>%
  mutate(WSPGrowth = 100*((avgWSP - lag(avgWSP,1))/lag(avgWSP,1))/(Year - lag(Year,1) )) %>% 
  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) )) %>%
  mutate(YieldGrowth = Yield - lag(Yield,1)) %>%
  mutate(ydGrowthPercent =  100*(YieldGrowth/lag(Yield,1))/(Year - lag(Year,1) )  ) %>%
  summarise(C2AAGR = mean(C2Growth, na.rm = TRUE),
            WSPAAGR = mean(WSPGrowth, na.rm = TRUE),
            avgAnnualYieldGrowth = mean(YieldGrowth, na.rm = TRUE),
            avgAnnualYieldGrowthPercent = mean(ydGrowthPercent, na.rm = TRUE),
            avgAnnualProfitMarginPercent = mean(marginPercent, na.rm = TRUE))
