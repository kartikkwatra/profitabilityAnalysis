library(tidyverse)

groundnutCost <- readxl::read_xlsx("kharif.xlsx",sheet = "groundnut" )

# Fiscal year set.
groundnutCost <- groundnutCost %>% mutate(Year = substr(Year,1,4) )

# State wise % net return over C2
groundnutCost %>% filter(State != "Orissa", State != "Orrisa"  ) %>%
  ggplot() +
  geom_bar(aes( State, percentNetReturnOverC2FHP , fill = Year), stat = "identity", position = "dodge" )

# Removing Orissa
groundnutCost <- groundnutCost %>% filter(State != "Orissa", State != "Orrisa"  )


# State wise % gross return over A2FL
groundnutCost %>%
  ggplot() +
  geom_bar(aes( State, percentGrossReturnOverA2FLFHP , fill = Year), stat = "identity", position = "dodge" )

# Average annual growth rates of C2 and FHP
x <- groundnut %>% group_by(State) %>%
  arrange(State,Year) %>%
  (gr = 100*(C2 - lag(C2,1))/lag(C2,1)  )
  
x <- groundnut %>% filter(State == "Maharashtra") %>%
  arrange(Year) %>%
  mutate(grC2 = 100*(C2 - lag(C2,1))/lag(C2,1), grFHP = 100*(FHP - lag(FHP,1))/lag(FHP,1)  ) 

mean(x$grFHP, na.rm = TRUE)
mean(x$grC2, na.rm = TRUE)

