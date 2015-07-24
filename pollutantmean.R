
pollutantmean <- function(directory,pollutant, id=1:332){

  try(setwd(directory), silent = TRUE) #change directory if needed, ignore if not
  
  pollvals<- data.frame() #create a blank frame to start with
  
  for(i in id) {
    j <- sprintf("%03d",i) #turn the ID into a 3 digit with leading zeroes
    b <- toString(j) #turn the three digit ID into a string
    d <- paste0(b,".csv") #make the filename full
    a <- read.csv(d) #open and read the file from earlier name
    pollvals <- rbind(pollvals,a) #append the files to the original
  }

fixpollutant <- (noquote(pollutant))
mean(pollvals[[fixpollutant]], na.rm=TRUE)

}
pollutantmean("specdata","sulfate",1:10)
