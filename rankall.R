# Assignment3 Part 3

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

rankall <- function(disease, num){
  if(disease == "heart attack")
    outcome <- 'HeartAttackMort'
  else if(disease == "pneumonia")
    outcome  <- 'PneumoniaMort'
  else if(disease == "heart failure")
    outcome <- 'HeartFailMort'
  else stop ("invalid outcome")
  


  #Filter for complete outcome, then sort by state, outcome, hospital name
  filtered <<- outcm_short[complete.cases(outcm_short[,outcome]),]
  sorted <<- filtered[order(filtered$State, filtered[outcome], filtered$Hospital.Name),]
  
  
  #Create a looping subfunction that will go through each state
  
  #Set up list of states for reference
  
  states <<- c("AK","AL","AR","AZ", "CA", "CO", "CT","DC","DE", "FL", "GA", "HI", 
               "IA", "ID","IL","IN","KS","KY","LA","MA",
              "MD","ME","MI","MN", "MO", "MS",
              "MT","NC", "NE","NH","NJ","NM", "NV", 
              "NY","ND","OH","OK","OR", "PA", "RI",
              "SC","SD", "TN", "TX","UT", "VA", "VT", "WA","WI",
              "WV","WY")

  #create a blank matrix to hold the output
  outputframe <<- data.frame(Hospital.Name=character(),
                             City=character(),
                             State=character(),
                             HeartAttackMort=numeric(),
                             HeartFailMort=numeric(),
                             PneumoniaMort=numeric()
                             )
  
  #loop through each item in states list
  for(item in states){
    ST<-item
    #subset the big list to just the state being iterated
      stfilt <- subset(sorted, State==ST)
    
    #count up the rows for just that state in list
      countrows <- nrow(stfilt)

    #number management to allow for best and worst
      if(num <= countrows){
        choice <- num}
   else if (num == "best")
     {choice <- 1}
   else if (num == "worst")
     {choice <- as.numeric(countrows)}
  else if (num > countrows){
      missingdf <- data.frame(Hospital.Name="NA",
                              City="NA",
                              State=ST,
                              HeartAttackMort=NA,
                              HeartFailMort=NA,
                              PneumoniaMort=NA)
     }
 #pull the desired row for the state out of the set
      
      #append that row to the output frame
      #If the value is missing for that state, then just append missing
      
    if(!exists("missingdf")){
      chosenhosp <- stfilt[choice,] 
      outputframe <<- rbind(outputframe, chosenhosp)
      }
    else{
      outputframe <<- rbind(outputframe, missingdf)
      rm(missingdf)
      }

  

  
  } 
  #just formatting and aesthetics
    results <- subset(outputframe, select= c(Hospital.Name, State))
  colnames(results)[1]<-"hospital"
  colnames(results)[2]<-"state"
  return(results)
 }


tail(rankall("pneumonia", "worst"),3)


