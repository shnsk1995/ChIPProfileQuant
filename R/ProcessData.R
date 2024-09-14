
ProcessData <- function(dataPath){

  # Read and process the file (if it exists)
  if (file.exists(dataPath)) {
    data <- read.delim(dataPath,header = FALSE)
    data <- as.data.frame(t(as.matrix(data)))
    print("File read.")
  } else {
    print("File does not exist.")
    return(NULL)
  }


  # Read the data from the file
  colnames(data)  <-  data[1,]
  data  <-  data[-(1:2),]
  data$bins  <-  as.integer(data$bins)

  return(data)

}
