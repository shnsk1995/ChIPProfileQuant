
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

  if (!require("ggplot2", quietly = TRUE)){
    install.packages("ggplot2")
  }

  if (!require("gridExtra", quietly = TRUE)){
    install.packages("gridExtra")
  }

  if (!require("tidyr", quietly = TRUE)){
    install.packages("tidyr")
  }

}



LoadRequiredPackages <- function(){

  library(readr)
  library(dplyr)
  library(optparse)
  library(ggplot2)
  library(gridExtra)
  library(tidyr)

}
