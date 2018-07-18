test <- groundnutPrices %>%
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

warnings()
as.numeric( subset(harvestSummary, Crop == cropName & State == "Gujarat", select = num) )

str(harvestSummary)

case_when()


semi_join()