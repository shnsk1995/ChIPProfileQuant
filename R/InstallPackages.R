
InstallRequiredPackages <- function(){

  #Install required packages if not installed already
  if (!require("readr", quietly = TRUE)){
    install.packages("readr")
  }

  if (!require("dplyr", quietly = TRUE)){
    install.packages("dplyr")
  }

  if (!require("optparse", quietly = TRUE)){
    install.packages("optparse")
  }

}



LoadRequiredPackages <- function(){

  library(readr)
  library(dplyr)
  library(optparse)

}
