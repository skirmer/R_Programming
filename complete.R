complete <- function(directory, id=1:332) {
  try(setwd(directory), silent = TRUE) #change directory if needed, ignore if not
  
  summarywork <- data.frame(x=numeric(0),y=numeric(0) ) #create a blank frame to start with
  colnames(summarywork) <- c('id','nobs')
  
  for(i in id) {
    j <- sprintf("%03d",i) #turn the ID into a 3 digit with leading zeroes
    b <- toString(j) #turn the three digit ID into a string
    d <- paste0(b,".csv") #make the filename full
    a <- read.csv(d) #open and read the file from earlier name
    completefile <- a[complete.cases(a), ] #produce a file of the complete cases for each
    sumfile <- data.frame(i,nrow(completefile)) #figure out how to build the small frame
    colnames(sumfile) <- c('id','nobs')
    summarywork <- rbind(summarywork, sumfile)
    
  }
  
  sumfile
  summarywork
}

complete ("specdata",c(2,4,6,8))
