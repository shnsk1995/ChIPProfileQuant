source("R/InstallPackages.R")
source("R/ProcessData.R")
source("R/Variables.R")
source("R/Quantification_ChIPseq.R")

QuantifyPeaks <- function(dataPath,control){

  if(is.null(dataPath)) return(print("Please provide average profile data from plotProfile function of deeptools"))

  if(is.null(control)) return(print("Please provide name of the control sample"))

  #Install all required packages
  InstallRequiredPackages()

  #Load required packages
  LoadRequiredPackages()

  #Process data
  data <- ProcessData(dataPath)

  #Calculate required variables
  downStreamRange <- CalculateDownRanges(data)
  upStreamRange <- CalculateUpRanges(data)
  totalBins  <- CalculateTotalBins(data)

  #Remove excess noise
  data  <-  data[-(totalBins+1:nrow(data)),]

  #Convert data into numeric data
  for (col in 3:ncol(data)){
    data[,col]  <-  as.numeric(data[,col])
  }

  #Quantify peaks
  resTab <- CalculatePeakRange(data,control,downStreamRange,upStreamRange)

  #Return the result table
  return(resTab)

}
