library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(naniar)

res <- GET("https://api.covidtracking.com/v1/states/daily.json")

states_data <- fromJSON(rawToChar(res$content))

view(states_data)



AL <- states_data %>% 
  filter(state == "AL") %>% 
  select(date, positive, negative, state, probableCases, 
         totalTestResults, hospitalizedCurrently, death)

vis_miss(AL)


WA <- states_data %>% 
  filter(state == "WA") %>% 
  select(date, positive, state, probableCases, 
         totalTestResults, hospitalizedCurrently, death, )
vis_miss(WA)



#The dataset is records data from Jan 2020 to Mar 2021 for all
#states and territories

unique(states_data$state)
# Contains multiple observations for each state & territory 

names(states_data)
# Contains 56 variables. Which to keep and which to drop?

#KEEP:
#date, state, negative, positive, hospitilized, hospitilizedCumulative, 
#hospitlizedCurrently, hospitalizedIncrease, negativeincrease, probablecases
#recovered, total, positiveIncrease



#DISCARD:
#commerical score
#dataQualityGrade 
#checkTimeEt
#dataModified
#deathConfirmed 
#deathProbable
#fips
#grade
#hash
#positiveTestsAntibody
#positiveTestsAntigen 
#positiveTestsPeopleAntibody
#lastUPdateEt
#negativeRegularScore
#negativeScore
#negativeTestsAntibody
#negativeTestsPeopleAntibody
#negativeTestsViral
#onVentilatorCumaltive
#onVentilatorCurrently
+pending
#posNeg
#positiveCasesViral
#positiveIncrease
#positiveScore



