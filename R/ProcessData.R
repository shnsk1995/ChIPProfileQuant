
ProcessData <- function(dataPath){

  data <- read.delim(dataPath,header = FALSE)
  data <- as.data.frame(t(as.matrix(data)))
  print("File read.")

  # Read the data from the file
  colnames(data)  <-  data[1,]
  data  <-  data[-(1:2),]
  data$bins  <-  as.integer(data$bins)

  return(data)

}
