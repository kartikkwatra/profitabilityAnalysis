## bajra Everything
library(tidyverse)
library(rvest)
library(lubridate)

# Get the data from file
bajraCost <- readxl::read_xlsx("kharif.xlsx",sheet = "bajra" )


#bajraWSP <- bajraPricesSubset %>% group_by(fiscal,State)
#temp <- semi_join(bajra,bajraCost, by = c("State" = "State", "fiscal" = "Year"))
unique(bajraCost$State)


# Getting the prices from file
bajraPrices <- data.frame()
cropName <- c("Bajra(Pearl Millet/Cumbu)")
monthList <- c('September', 'October', 'November','June','July')
yearList <- as.character(seq(2006,2015, by=1))

bajraPrices <- combine_data(cropName,yearList,monthList,bajraPrices,fmonth = 5)
bajraPrices <- clean_data(bajraPrices)
bajraPrices <- bajraPrices %>% rename(State = X1,Price = X2, Month = month) 
bajraPrices$Price <- as.numeric(bajraPrices$Price)
# No of months of Harvest for each crop
monthMatrix <- readxl::read_xlsx("matrix.xlsx")
harvestSummary <- monthMatrix %>% group_by(Crop,State) %>% summarise(num = n())

cropName <- c("Bajra")
# Getting list of states which have atleast x price points per and with available costs
bajraPricesSubsetLarge <- bajraPrices %>%
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
str(bajraPricesSubsetLarge)
bajraPricesSubsetLarge$Price <- as.numeric(bajraPricesSubsetLarge$Price)
temp <- bajraPricesSubsetLarge %>% 
  summarise(min = min(Price), max = max(Price))


# Now Cleaning for harvest
bajraPricesSubsetSmall <-  filter_by_harvest(cropName,bajraPricesSubsetLarge)

# Imp to check or filter only those groups where the minimum number of data points is satisfied
bajraPricesSubsetSmaller <- bajraPricesSubsetSmall %>%
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
bajraPricesSubsetSmaller$Price <- as.numeric(bajraPricesSubsetSmaller$Price)
bajraPricesSubsetSmaller$fiscal <- as.integer(bajraPricesSubsetSmaller$fiscal)
bajraWSPSmaller <- bajraPricesSubsetSmaller %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

bajraPricesSubsetLarge$Price <- as.numeric(bajraPricesSubsetLarge$Price)
bajraPricesSubsetLarge$fiscal <- as.integer(bajraPricesSubsetLarge$fiscal)
bajraWSPLarge <- bajraPricesSubsetLarge %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

# Found a difference in the number of cases b/w bajraWSPSmaller and Large, so joining
temp <- anti_join(bajraWSPLarge,bajraWSPSmaller, by= c("State","fiscal"))
bajraWSP <- rbind(bajraWSPSmaller,temp)


# Now joining the Cost and WSP tables: Our Final Table
bajra <- inner_join(bajraCost,bajraWSP, by = c("State" = "State",  "Year" = "fiscal" ))


###### ANALYSIS ########

# be careful of periods: should be comparable for both  
# Average annual growth rates of C2
#bajraCost$Year <- as.numeric(bajraCost$Year)
#bajraCost <- bajraCost %>% group_by(State) %>%
#  arrange(State,Year) %>%
#  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

## net margins

bajra <- bajra %>% filter(State != "Tamil Nadu") %>%
  mutate( margin = avgWSP - C2, marginPercent = 100*margin/C2)

baj <- bajra %>% 
  ggplot() +
  geom_bar( aes(x = State, y = marginPercent,
                fill = factor(Year),
                alpha = factor(Year),
                width = 0.6
  ) , stat = "identity", position = "dodge") +
  labs(x = "States", y = "Percentage Net Return in Rs/Quintal",
       title = "Net Returns in Bajra growing states",
       fill = "Year", alpha = "Year")

ggplotly(baj)

# Average annual growth rates of avgWSP and C2 for the same period
bajra <- bajra %>% group_by(State) %>%
  arrange(State,Year) %>%
  mutate(WSPGrowth = 100*((avgWSP - lag(avgWSP,1))/lag(avgWSP,1))/(Year - lag(Year,1) )) %>% 
  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

bajra %>% group_by(State) %>%
  summarise(C2AAGR = mean(C2Growth, na.rm = TRUE),
            WSPAAGR = mean(WSPGrowth, na.rm = TRUE),
            avgAnnualProfitMarginPercent =  mean(marginPercent, na.rm = TRUE) )
