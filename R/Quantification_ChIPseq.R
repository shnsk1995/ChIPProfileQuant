if (!require("readr", quietly = TRUE)){
  install.packages("readr")
}

if (!require("dplyr", quietly = TRUE)){
  install.packages("dplyr")
}

if (!require("optparse", quietly = TRUE)){
  install.packages("optparse")
}


library(readr)
library(dplyr)
library(optparse)

# Define the command-line options
option_list <- list(
  make_option(c("-f", "--file"), type = "character",
              help = "Path to the input file", metavar = "character"),
  make_option(c("-c", "--control"), type = "character",
              help = "Control sample name", metavar = "character"),
  make_option(c("-o", "--outdir"), type = "character",
              help = "Path to the output file", metavar = "character")
)

# Parse the options
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Check if the file argument is provided
if (is.null(opt$file)) {
  stop("No file path provided. Use -f or --file to specify the file path.")
}

# Check if the control sample name argument is provided
if (is.null(opt$control)) {
  stop("No control sample name provided. Use -c or --control to specify the control sample name.")
}

# Access the file path
file_path <- opt$file
print(paste("File path passed:", file_path))

# Read and process the file (if it exists)
if (file.exists(file_path)) {
  data <- read.delim(file_path,header = FALSE)
  data <- as.data.frame(t(as.matrix(data)))
  print("File read.")
} else {
  print("File does not exist.")
}


# Read the data from the file
colnames(data) <- data[1,]
data <- data[-(1:2),]
data$bins <- as.integer(data$bins)

binLabs <- data$`bin labels`
binLabsComplete <- binLabs[complete.cases(binLabs)]

downStreamRange <- binLabsComplete[1]
if(any(grepl("K",downStreamRange))){
  intBpsDownStream <- as.numeric(abs(as.numeric(sub("\\K.*","",downStreamRange)))) * 1000
}else{
  intBpsDownStream <- as.numeric(abs(as.numeric(sub("b.*","",downStreamRange))))
}


upStreamRange <- binLabsComplete[3]
if(any(grepl("K",upStreamRange))){
  intBpsUpStream <- as.numeric(abs(as.numeric(sub("\\K.*","",upStreamRange)))) * 1000
}else{
  intBpsUpStream <- as.numeric(abs(as.numeric(sub("b.*","",upStreamRange))))
}


totalBins <- nrow(data)/(length(binLabsComplete)/3)

data <- data[-(totalBins+1:nrow(data)),]

for (col in 3:ncol(data)){
  data[,col] <- as.numeric(data[,col])
}


controlSample <- opt$control



CalculatePeakRange <- function(data,control,downStream,upStream,bpSize){

  controlSample <- control
  noOfSamples <- ncol(data) - 2


  comResultTable <- as.data.frame(matrix(ncol = 10,nrow = 0))
  colnames(comResultTable)<-c("Sample","PeakBin","PeakStartBin","PeakEndBin","PeakWidth","PeakWidthChange","PeakScore","PeakScoreChange","PeakShift","WindowSize")

  bpsPerBin <- (upStream+downStream)/max(data$bins)
  minWindowSize <- 30/bpsPerBin
  maxWindowSize <- 100/bpsPerBin


  for(windowSize in minWindowSize:maxWindowSize){

    if(!(upStream+downStream)%%windowSize==0){
      while ((upStream+downStream)%%windowSize!=0) {

        windowSize <- windowSize + 1

      }
    }


    if(windowSize %in% comResultTable$WindowSize) next

    resultTable<-as.data.frame(matrix(ncol = 6,nrow = 0))
    colnames(resultTable)<-c("Sample","PeakBin","PeakStartBin","PeakEndBin","PeakWidth","PeakScore")

    for(i in 1:noOfSamples){

      df <- data.frame(`bin labels`=data$`bin labels`,bins=data$bins,data_column=data[,2+i])
      sampleName <- as.character(colnames(data)[2+i])
      colnames(df) <- c("bin labels","bins",sampleName)
      # Calculate the rate of change
      df <- df %>%
        arrange(bins) %>%  # Ensure data is sorted by time
        mutate(
          Difference = !!sym(sampleName) - first(!!sym(sampleName)),
          ChangePercentage = (Difference / first(!!sym(sampleName))) * 100,
          PreviousBinPercentage = 0,
          RateOfChangePercentage = 0
        )

      bin=1

      for(i in 1:nrow(df)){
        if(i==1 && bin==1){
          df$PreviousBinPercentage[i] <- 0
        }else if(i%%windowSize==1){
          df$PreviousBinPercentage[i] <- df$ChangePercentage[i-windowSize]
        }else{
          df$PreviousBinPercentage[i] <- df$ChangePercentage[bin]
        }
        df$RateOfChangePercentage[i] <- df$ChangePercentage[i]-df$PreviousBinPercentage[i]
        if(i %% windowSize == 0){bin <- bin + windowSize}
      }


      PeakBin=df[df[,3]==max(df[,3]),]$bins
      Starting_bin <- 1
      last_window <- c()
      for(i in seq(Starting_bin,PeakBin-(PeakBin%%windowSize),by = windowSize)){
        j=i
        k=i+(windowSize-1)
        while(j<k) {
          if(j!=1 && j==i){
            if(df$RateOfChangePercentage[j]<df$RateOfChangePercentage[j-1]){
              last_window <- c()
            }
            j <- j+1
          }

          if(j==1){
            j <- j+1
          }

          if(!df$RateOfChangePercentage[j]<df$RateOfChangePercentage[j+1]){
            Starting_bin <- i+windowSize
            last_window <- c(last_window,FALSE)
            break
          }
          j <- j+ 1
        }
        if(FALSE %in% last_window){
          last_window <- c()
        }else{
          last_window <- c(last_window,TRUE)
        }
      }


      Ending_bin <- PeakBin+((windowSize+1)-(PeakBin%%windowSize))
      last_window <- c()
      for(i in seq(Ending_bin,nrow(df)-1,by = windowSize)){
        j=i
        k=i+(windowSize-1)

        while(j<k) {
          if(j!=(PeakBin+((windowSize+1)-(PeakBin%%windowSize))) && j==i){
            if(df$RateOfChangePercentage[j]>df$RateOfChangePercentage[j-1]){
              last_window <- c()
            }
            j <- j+1
          }

          if(j==(PeakBin+((windowSize+1)-(PeakBin%%windowSize)))){
            j <- j+1
          }

          if(!df$RateOfChangePercentage[j]>df$RateOfChangePercentage[j+1]){
            Ending_bin <- i
            last_window <- c(last_window,FALSE)
            break
          }
          j <- j+ 1
        }
        if(FALSE %in% last_window){
          last_window <- c()
          break
        }else{
          last_window <- c(last_window,TRUE)
        }
      }


      sampleRow <- data.frame(Sample=sampleName,
                              PeakBin=df[df[,3]==max(df[,3]),]$bins,
                              PeakStartBin=Starting_bin,
                              PeakEndBin=Ending_bin,
                              PeakWidth=(Ending_bin-Starting_bin)*bpsPerBin,
                              PeakScore=max(df[,3])
      )

      resultTable <- rbind(resultTable,sampleRow)

    }


    resultTable <- resultTable %>%
      mutate(PeakScoreChange = ifelse(Sample==control,
                                      "Control",
                                      paste0(round(((PeakScore-first(PeakScore))/first(PeakScore))*100,2),"%")))

    resultTable <- resultTable %>%
      mutate(PeakWidthChange = ifelse(Sample==control,
                                      "Control",
                                      paste0(round(((PeakWidth-first(PeakWidth))/first(PeakWidth))*100,2),"%")))

    resultTable <- resultTable %>%
      mutate(PeakShift = ifelse(Sample==control,"Control",ifelse(PeakBin==first(PeakBin),
                                                                 "No Shift",
                                                                 ifelse(PeakBin<first(PeakBin),
                                                                        paste0("Peak shifted left by ",(PeakBin-first(PeakBin))*bpsPerBin," bps"),
                                                                        paste0("Peak shifted right by ",(PeakBin-first(PeakBin))*bpsPerBin," bps"))
      )))


    resultTable <- resultTable %>%
      mutate(WindowSize = windowSize)

    resultTable<- resultTable[c("Sample","PeakBin","PeakStartBin","PeakEndBin","PeakWidth","PeakWidthChange","PeakScore","PeakScoreChange","PeakShift","WindowSize")]


    comResultTable <-  rbind(comResultTable,resultTable)


  }

  resultTable<-as.data.frame(matrix(ncol = 6,nrow = 0))
  colnames(resultTable)<-c("Sample","PeakBin","PeakStartBin","PeakEndBin","PeakWidth","PeakScore")

  for (sam in unique(comResultTable$Sample)) {

    samDF <- comResultTable[comResultTable$Sample==sam,]

    sampleRow <- data.frame(Sample=sam,
                            PeakBin=samDF$PeakBin[1],
                            PeakStartBin=as.integer(mean(samDF$PeakStartBin)),
                            PeakEndBin=as.integer(mean(samDF$PeakEndBin)),
                            PeakWidth=as.integer(mean(samDF$PeakWidth)),
                            PeakScore=samDF$PeakScore[1]
    )

    resultTable <- rbind(resultTable,sampleRow)



  }

  resultTable <- resultTable %>%
    mutate(PeakScoreChange = ifelse(Sample==control,
                                    "Control",
                                    paste0(round(((PeakScore-first(PeakScore))/first(PeakScore))*100,2),"%")))

  resultTable <- resultTable %>%
    mutate(PeakWidthChange = ifelse(Sample==control,
                                    "Control",
                                    paste0(round(((PeakWidth-first(PeakWidth))/first(PeakWidth))*100,2),"%")))

  resultTable <- resultTable %>%
    mutate(PeakShift = ifelse(Sample==control,"Control",ifelse(PeakBin==first(PeakBin),
                                                               "No Shift",
                                                               ifelse(PeakBin<first(PeakBin),
                                                                      paste0("Peak shifted left by ",(PeakBin-first(PeakBin))*bpsPerBin," bps"),
                                                                      paste0("Peak shifted right by ",(PeakBin-first(PeakBin))*bpsPerBin," bps"))
    )))




  return(resultTable)

}


resTab <- CalculatePeakRange(data,controlSample,intBpsDownStream,intBpsUpStream)


if (is.null(opt$outdir)) {
  write_tsv(resTab,file = "PeakResultTable.txt")
}else{
  write_tsv(resTab,file = paste0(opt$outdir,"PeakResultTable.txt"))
}

