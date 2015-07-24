corr <- function(directory, threshold = 0) {
  library(stats) # call the stats library so that cor is available
  try(setwd(directory), silent = TRUE) #change directory if needed, ignore if not
  
  allfiles <- list.files() #tell system which files to take
  corrfile<- vector() 
  
  for(file in allfiles){
    newfile <- read.csv(file, header=TRUE) #read the file
    newfile2 <- newfile[complete.cases(newfile), ] 
    if(threshold < nrow(newfile2)){ # test for the threshold and see what we have for correlations if it's met
      results <- cor(newfile2$nitrate, newfile2$sulfate) # run the correlation
      corrfile<- c(corrfile, results) #append the correlation value to the vector
    }
  }
  
  if(length(corrfile) ==0){
    corrfile<-vector()
  }
  #nrow(mergedfiles)
  #head(mergedfiles)
  corrfile
}  
cr <-corr("specdata", 150)
head(cr)
summary(cr)

cr <-corr("specdata", 400)
head(cr)
summary(cr)

cr <-corr("specdata", 5000)
summary(cr)
length(cr)

cr <-corr("specdata")
summary(cr)
length(cr)

