evencount <- function(x) {
  counter<-0
  for (n in x) {
    if (n %% 2 == 0) 
      counter <- counter+1
  }
  return(counter)
}

evencount(x)

sadlibs <- function() {
  first <- readline("Give a noun:")
  second <- readline("Give a verb:")
  third <- readline("Give an adjective:")
  fourth <- readline("Give an exclamation:")
  sentence <- cat(paste("The", first, "was very happy, so it",second," and found a very", third, "tree.", fourth, "Said the", first, "!"))  
  return(sentence)
}


sadlibs()