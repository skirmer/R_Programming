# Assignment3 Part 2

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

rankhospital <- function(ST,disease,num){
  
  #Check that the disease is right
    if(disease == "heart attack")
    outcome <- 'HeartAttackMort'
  else if(disease == "pneumonia")
    outcome  <- 'PneumoniaMort'
  else if(disease == "heart failure")
    outcome <- 'HeartFailMort'
  else stop ("invalid outcome")
  
  #Check that the state is right
  states <- c("AK","AL","AZ","AR", "CA", "CO", "CT","DE", "FL", "GA", "HI", 
              "ID","IL","IN","IA","KS","KY","LA","ME",
              "MD","MA","MI","MN", "MS","MO",
              "MT","NE","NV","NH","NJ", "NM",
              "NY","NC","ND", "OH","OK","OR", "PA", "RI",
              "SC","SD", "TN", "TX","UT", "VT","VA", "WA",
              "WV","WI", "WY")
  
  if(!ST %in% states) 
    stop ("invalid state")
  
  #filter and sort based on disease and state
  filtered <<- subset(outcm_short, State==ST)
  filtered2 <<- filtered[complete.cases(filtered[,outcome]),]
  sorted <<- filtered2[order(filtered2[outcome], filtered2$Hospital.Name),]
  
  #check that the number is right

  countrows <- nrow(sorted)
  
  if(num <= countrows)
    choice <- num
  else if (num == "best")
    choice <- 1
  else if (num == "worst")
    choice <- as.numeric(countrows)
  else if (num > countrows)
    return ("NA")
  

  #grab the hospital and return it
  tophosp <<- sorted[choice,]
  return(as.character(tophosp$Hospital.Name))
  
  #   tophosp2 <<- sorted[1,]
  #   return(tophosp2$Hospital.Name)
}

rankhospital("MD","heart attack", "worst")