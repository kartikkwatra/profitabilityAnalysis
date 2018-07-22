## Gram Everything
library(tidyverse)
library(rvest)
library(lubridate)

# Get the data from file
gramCost <- readxl::read_xlsx("rabi.xlsx",sheet = "gram" )


#gramWSP <- gramPricesSubset %>% group_by(fiscal,State)
#temp <- semi_join(gram,gramCost, by = c("State" = "State", "fiscal" = "Year"))
unique(gramCost$State)


# Getting the prices from file
gramPrices <- data.frame()
cropName <- c("Bengal Gram(Gram)(Whole)")
monthList <- c('January','February','March','April','May')
yearList <- as.character(seq(2006,2015, by=1))

gramPrices <- combine_data(cropName,yearList,monthList,gramPrices,fmonth = 5)
gramPrices <- clean_data(gramPrices)
gramPrices <- gramPrices %>% rename(State = X1,Price = X2, Month = month) 
gramPrices$Price <- as.numeric(gramPrices$Price)
# No of months of Harvest for each crop
monthMatrix <- readxl::read_xlsx("matrix.xlsx")
harvestSummary <- monthMatrix %>% group_by(Crop,State) %>% summarise(num = n())

cropName <- c("Bengal Gram")
# Getting list of states which have atleast x price points per and with available costs
gramPricesSubsetLarge <- gramPrices %>%
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
str(gramPricesSubsetLarge)
gramPricesSubsetLarge$Price <- as.numeric(gramPricesSubsetLarge$Price)
temp <- gramPricesSubsetLarge %>% 
  summarise(min = min(Price), max = max(Price))


# Now Cleaning for harvest
gramPricesSubsetSmall <-  filter_by_harvest(cropName,gramPricesSubsetLarge)

# Imp to check or filter only those groups where the minimum number of data points is satisfied
gramPricesSubsetSmaller <- gramPricesSubsetSmall %>%
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
gramPricesSubsetSmaller$Price <- as.numeric(gramPricesSubsetSmaller$Price)
gramPricesSubsetSmaller$fiscal <- as.integer(gramPricesSubsetSmaller$fiscal)
gramWSPSmaller <- gramPricesSubsetSmaller %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

gramPricesSubsetLarge$Price <- as.numeric(gramPricesSubsetLarge$Price)
gramPricesSubsetLarge$fiscal <- as.integer(gramPricesSubsetLarge$fiscal)
gramWSPLarge <- gramPricesSubsetLarge %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

# Found a difference in the number of cases b/w gramWSPSmaller and Large, so joining
temp <- anti_join(gramWSPLarge,gramWSPSmaller, by= c("State","fiscal"))
gramWSP <- rbind(gramWSPSmaller,temp)


# Now joining the Cost and WSP tables: Our Final Table
gram <- inner_join(gramCost,gramWSP, by = c("State" = "State",  "Year" = "fiscal" ))


###### ANALYSIS ########

# be careful of periods: should be comparable for both  
# Average annual growth rates of C2
#gramCost$Year <- as.numeric(gramCost$Year)
#gramCost <- gramCost %>% group_by(State) %>%
#  arrange(State,Year) %>%
#  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

## net margins

gram <- gram %>% filter( State != "Haryana", State != "Jharkhand") %>%
  mutate( margin = avgWSP - C2, marginPercent = 100*margin/C2)
 
gr <- gram %>% 
 ggplot() +
  geom_bar( aes(x = State, y = marginPercent,
                fill = factor(Year),
                alpha = factor(Year),
                width = 0.6
                ) , stat = "identity", position = "dodge") +
  labs(x = "States", y = "Percentage Net Return in Rs/Quintal",
       title = "Net Returns in Gram growing states",
       fill = "Year", alpha = "Year")

ggplotly(gr)

# Average annual growth rates of avgWSP and C2 for the same period
gram <- gram %>% group_by(State) %>%
  arrange(State,Year) %>%
  mutate(WSPGrowth = 100*((avgWSP - lag(avgWSP,1))/lag(avgWSP,1))/(Year - lag(Year,1) )) %>% 
  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

gram %>% group_by(State) %>%
  summarise(C2AAGR = mean(C2Growth, na.rm = TRUE),
            WSPAAGR = mean(WSPGrowth, na.rm = TRUE),
            avgAnnualProfitMarginPercent =  mean(marginPercent, na.rm = TRUE) )
