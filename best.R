# Assignment3

#Get into the right directory
getwd()
# setwd('Assignment3')

#open up the source data
hospitals <- read.csv('hospital-data.csv')
outcomes <- read.csv('outcome-of-care-measures.csv', header=TRUE, 
                     na.strings="Not Available")


#Create a subset of the original datasets to make it easier to work with

hosp_short <- subset(hospitals, select = c(Hospital.Name, City, State))
outcm_short <- subset(outcomes, select = c(Hospital.Name, City, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))

#Rename the outcome columns to be more useful
colnames(outcm_short)[4]<- "HeartAttackMort"
colnames(outcm_short)[5]<- "HeartFailMort"
colnames(outcm_short)[6]<- "PneumoniaMort"

# outcm_short$HeartAttackMort <- as.numeric(outcm_short$HeartAttackMort)
# outcm_short$HeartFailMort <- as.numeric(outcm_short$HeartFailMort)
# outcm_short$PneumoniaMort <- as.numeric(outcm_short$PneumoniaMort)

#hospital.name is filtered[2]

#filter down to the desired state
best <- function(ST,disease){
  if(disease == "heart attack")
    outcome <- 'HeartAttackMort'
  else if(disease == "pneumonia")
    outcome  <- 'PneumoniaMort'
  else if(disease == "heart failure")
    outcome <- 'HeartFailMort'
  else stop ("invalid outcome")

  states <- c("AK","AL","AZ","AR", "CA", "CO", "CT","DE", "FL", "GA", "HI", 
              "ID","IL","IN","IA","KS","KY","LA","ME",
              "MD","MA","MI","MN", "MS","MO",
              "MT","NE","NV","NH","NJ", "NM",
              "NY","NC","ND", "OH","OK","OR", "PA", "RI",
              "SC","SD", "TN", "TX","UT", "VT","VA", "WA",
              "WV","WI", "WY")
  
  if(!ST %in% states) 
    stop ("invalid state")
  
  filtered <<- subset(outcm_short, State==ST, !is.na(outcome))
  sorted <<- filtered[order(filtered[outcome], filtered$Hospital.Name),]
  tophosp <<- as.character(head(sorted$Hospital.Name,1))
  return(tophosp)
#   tophosp2 <<- sorted[1,]
#   return(tophosp2$Hospital.Name)
  
}

best('TX','heart failure')

