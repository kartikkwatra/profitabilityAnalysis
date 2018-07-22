# Arhar Everything
library(tidyverse)
library(rvest)
library(lubridate)

# Get the data from file
arharCost <- readxl::read_xlsx("kharif.xlsx",sheet = "arhar" )

# Fiscal year set.
arharCost <- arharCost %>% mutate(Year = substr(Year,1,4) )
unique(arharCost$State)

# Removing Orissa
#arharCost <- arharCost %>% filter(State != "Orissa", State != "Orrisa"  )

# Getting the prices from file
arharPrices <- data.frame()
cropName <- c("Arhar (Tur/Red Gram)(Whole)")
monthList <- c('October', 'November', 'December', 'January','February','March','April','May' )
yearList <- as.character(seq(2006,2015, by=1))

arharPrices <- combine_data(cropName,yearList,monthList,arharPrices,fmonth = 5)
arharPrices <- clean_data(arharPrices)
arharPrices <- arharPrices %>% rename(State = X1,Price = X2, Month = month) 

# No of months of Harvest for each crop
monthMatrix <- readxl::read_xlsx("matrix.xlsx")
harvestSummary <- monthMatrix %>% group_by(Crop,State) %>% summarise(num = n())

# Getting list of states which have atleast x price points per and with available costs
arharPricesSubsetLarge <- arharPrices %>%
  group_by(fiscal,State) %>%
  filter( any( 
    n() >=
      case_when(
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 1 ~ 1,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 2 ~ 1,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 3 ~ 2,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 4 ~ 2,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 5 ~ 3,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 6 ~ 3,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 7 ~ 3,
        as.numeric(harvestSummary[ (harvestSummary$Crop == cropName) & (harvestSummary$State == State) , 3]) == 8 ~ 4
        #TRUE ~ 100
      )
  )
  )

# Checking for Min Max - Ideally should happen earlier: Just after creating the subset
str(arharPricesSubsetLarge)
arharPricesSubsetLarge$Price <- as.numeric(arharPricesSubsetLarge$Price)
temp <- arharPricesSubsetLarge %>% group_by(State, fiscal) %>% 
  summarise(min = min(Price), max = max(Price))


#arharWSP <- arharPricesSubset %>% group_by(fiscal,State)
#temp <- semi_join(arhar,arharCost, by = c("State" = "State", "fiscal" = "Year"))
#unique(temp$State)

# Now Cleaning for harvest
arharPricesSubsetSmall <-  filter_by_harvest(cropName,arharPricesSubsetLarge)

# Imp to check or filter only those groups where the minimum number of data points is satisfied
arharPricesSubsetSmaller <- arharPricesSubsetSmall %>%
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
arharPricesSubsetSmaller$Price <- as.numeric(arharPricesSubsetSmaller$Price)
arharPricesSubsetSmaller$fiscal <- as.integer(arharPricesSubsetSmaller$fiscal)
arharWSPSmaller <- arharPricesSubsetSmaller %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

arharPricesSubsetLarge$Price <- as.numeric(arharPricesSubsetLarge$Price)
arharPricesSubsetLarge$fiscal <- as.integer(arharPricesSubsetLarge$fiscal)
arharWSPLarge <- arharPricesSubsetLarge %>% 
  summarise(avgWSP = mean(Price, na.rm = TRUE))

# Found a difference in the number of cases b/w arharWSPSmaller and Large, so joining
temp <- anti_join(arharWSPLarge,arharWSPSmaller, by= c("State","fiscal"))
arharWSP <- rbind(arharWSPSmaller,temp)


arharCost$Year <- as.numeric(arharCost$Year)
# Now joining the Cost and WSP tables: Our Final Table
arhar <- inner_join(arharCost,arharWSP, by = c("State" = "State",  "Year" = "fiscal" ))


###### ANALYSIS ########

# be careful of periods: should be comparable for both  
# Average annual growth rates of C2
#arharCost$Year <- as.numeric(arharCost$Year)
#arharCost <- arharCost %>% group_by(State) %>%
#  arrange(State,Year) %>%
#  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

## net margins

arhar <- arhar %>% filter(State != "Andhra Pradesh") %>%
  mutate( margin = avgWSP - C2, marginPercent = 100*margin/C2)

ar <- arhar %>%
  ggplot() +
  geom_bar( aes(x = State, y = marginPercent,
                fill = factor(Year), alpha = factor(Year), width = 0.6 ),
            stat = "identity", position = "dodge") +
  labs(x = "States", y = "Percentage Net Return in Rs/Quintal",
       title = "Net Returns in Arhar(Tur) growing states",
       fill = "Year", alpha = "Year")

ggplotly(ar)

  
# Average annual growth rates of avgWSP and C2 for the same period
arhar <- arhar %>% group_by(State) %>% filter(Year != "2009") %>%
  arrange(State,Year) %>%
  mutate(WSPGrowth = 100*((avgWSP - lag(avgWSP,1))/lag(avgWSP,1))/(Year - lag(Year,1) )) %>% 
  mutate(C2Growth = 100*((C2 - lag(C2,1))/lag(C2,1))/(Year - lag(Year,1) ))

arhar %>% group_by(State) %>%
  summarise(C2AAGR = mean(C2Growth, na.rm = TRUE), WSPAAGR = mean(WSPGrowth, na.rm = TRUE), avgAnnualProfitMarginPercent =  mean(marginPercent, na.rm = TRUE) )
