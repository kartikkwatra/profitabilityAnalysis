# Groundnut Everything
library(tidyverse)
library(rvest)
library(lubridate)

# Get the data from file
groundnutCost <- readxl::read_xlsx("kharif.xlsx",sheet = "groundnut" )

# Fiscal year set.
groundnutCost <- groundnutCost %>% mutate(Year = substr(Year,1,4) )

# Removing Orissa
#groundnutCost <- groundnutCost %>% filter(State != "Orissa", State != "Orrisa"  )

# Getting the prices from file
groundnutPrices <- data.frame()
cropName <- c("Groundnut pods (raw)")
monthList <- c('September', 'October', 'November', 'December')
yearList <- as.character(seq(2006,2014, by=1))

groundnutPrices <- combine_data(cropName,yearList,monthList,groundnutPrices,fmonth = 3)
groundnutPrices <- clean_data(groundnutPrices)
groundnutPrices <- groundnutPrices %>% rename(State = X1,Price = X2, Month = month) 

# No of months of Harvest for each crop
monthMatrix <- readxl::read_xlsx("matrix.xlsx")
harvestSummary <- monthMatrix %>% group_by(Crop,State) %>% summarise(num = n())

# Getting list of states which have atleast 2 price points per and with available costs
groundnutPricesSubsetLarge <- groundnutPrices %>%
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

# Checking for Min Max - Ideally should happen earlier: Just after creating the subset
str(groundnutPricesSubsetLarge)
groundnutPricesSubsetLarge$Price <- as.numeric(groundnutPricesSubsetLarge$Price)
temp <- groundnutPricesSubsetLarge %>% group_by(State, fiscal) %>% 
  summarise(min = min(Price), max = max(Price))


#groundnutWSP <- groundnutPricesSubset %>% group_by(fiscal,State)
#temp <- semi_join(groundnut,groundnutCost, by = c("State" = "State", "fiscal" = "Year"))
#unique(temp$State)

# Now Cleaning for harvest
groundnutPricesSubsetSmall <-  filter_by_harvest(cropName,groundnutPricesSubsetLarge)

# Imp to check or filter only those groups where the minimum number of data points is satisfied
groundnutPricesSubsetSmaller <- groundnutPricesSubsetSmall %>%
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
groundnutPricesSubsetSmaller$Price <- as.numeric(groundnutPricesSubsetSmaller$Price)
groundnutPricesSubsetSmaller$fiscal <- as.integer(groundnutPricesSubsetSmaller$fiscal)
groundnutWSPSmaller <- groundnutPricesSubsetSmaller %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

groundnutPricesSubsetLarge$Price <- as.numeric(groundnutPricesSubsetLarge$Price)
groundnutPricesSubsetLarge$fiscal <- as.integer(groundnutPricesSubsetLarge$fiscal)
groundnutWSPLarge <- groundnutPricesSubsetLarge %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

# Found no difference in the number of cases b/w groundnutWSPSmaller and Large, so using the former
temp <- anti_join(groundnutWSPLarge,groundnutWSPSmaller, by= c("State","fiscal"))
groundnutWSP <- groundnutWSPSmaller



# Now joining the Cost and WSP tables: Our Final Table
groundnutCost$Year <- as.numeric(groundnutCost$Year)
groundnut <- inner_join(groundnutCost,groundnutWSP, by = c("State" = "State",  "Year" = "fiscal" ))


###### ANALYSIS ########
# Average annual growth rates of avgWSP and C2 for the same period
groundnut <- groundnut %>% group_by(State) %>%
  arrange(State,Year) %>%
  mutate(WSPGrowth = 100*((avgWSP - lag(avgWSP,1))/lag(avgWSP,1))/(Year - lag(Year,1) )) %>% 
  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

# be careful of periods: should be comparable for both  

# Average annual growth rates of C2
#groundnutCost$Year <- as.numeric(groundnutCost$Year)
#groundnutCost <- groundnutCost %>% group_by(State) %>%
#  arrange(State,Year) %>%
#  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

## net margins

groundnut <- groundnut %>%
  mutate( margin = avgWSP - C2, marginPercent = 100*margin/C2, priceDiff = FHP- avgWSP)  


groundnut %>% group_by(State) %>%
  summarise(C2AAGR = mean(C2Growth, na.rm = TRUE), 
            WSPAAGR = mean(WSPGrowth, na.rm = TRUE),
            avgAnnualProfitMarginPercent =  mean(marginPercent, na.rm = TRUE)  )



gg <- groundnut %>%
  ggplot() +
  geom_col( aes(x = State, y = marginPercent,
                fill = factor(Year),alpha = factor(Year)
               ) , width = 0.7,
            stat = "identity",
            position = "dodge") +
  labs(x = "States", y = "Percentage Net Return in Rs/Quintal",
       title = "Net Returns in Groundnut growing states",
            fill = "Year", alpha = "Year")

gg
ggplotly(gg)
# Consistent trend in the difference in prices
groundnut %>%
  ggplot() +
  geom_bar( aes(x = State, y = priceDiff, fill = factor(Year), alpha = factor(Year %% 1900) ) , stat = "identity", position = "dodge")

#temp <- groundnut %>% group_by(State) %>% filter(!(Year %in% c("2016","2017")  ) ) %>%
#  arrange(State,Year) %>%
#  mutate(priceDiffGrowth = 100*((priceDiff - lag(priceDiff,1))/lag(priceDiff,1))/(Year - lag(Year,1) ))
#temp %>% group_by(State) %>%
#  summarise(PDAAGR = mean(priceDiffGrowth, na.rm = TRUE))

