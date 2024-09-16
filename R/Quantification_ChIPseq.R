CalculateRateOfChange <- function(df,sampleName,windowSize){

  # Calculate the rate of change
  df <- df %>%
    mutate(
      Difference = abs(!!sym(sampleName) - first(!!sym(sampleName))),
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

  return(df)

}



CalculatePeakStart <- function(df,windowSize){

  PeakBin=df[df[,3]==max(df[,3]),]$bins
  Starting_bin <- 1
  last_window <- c()
  for(i in seq(Starting_bin,PeakBin-windowSize,by = windowSize)){
    j=i
    k=i+(windowSize-1)
    while(j<k) {
      if(j!=1 && j==i){
        if(df$RateOfChangePercentage[j]<df$RateOfChangePercentage[j-1]){
          peakStart <- j
          last_window <- c()
        }
        j <- j+1
      }

      if(!df$RateOfChangePercentage[j]<df$RateOfChangePercentage[j+1]){

        peakStart <- j+1
        last_window <- c(last_window,FALSE)

      }
      j <- j+ 1
    }
    if(FALSE %in% last_window){
      last_window <- c()
    }else{
      last_window <- c(last_window,TRUE)
    }
  }

  return(peakStart)

}

CalculatePeakEnd <- function(df,windowSize){

  browser()

  df <- df%>%
    mutate(ProxyBins = 1:nrow(df))

  PeakBin=df[df[,3]==max(df[,3]),]$ProxyBins
  Starting_bin <- 1
  last_window <- c()
  for(i in seq(Starting_bin,PeakBin-windowSize,by = windowSize)){
    j=i
    k=i+(windowSize-1)
    while(j<k) {
      if(j!=1 && j==i){
        if(df$RateOfChangePercentage[j]<df$RateOfChangePercentage[j-1]){
          peakEnd <- j
          last_window <- c()
        }
        j <- j+1
      }

      if(!df$RateOfChangePercentage[j]<df$RateOfChangePercentage[j+1]){

        peakEnd <- j+1
        last_window <- c(last_window,FALSE)

      }
      j <- j+ 1
    }
    if(FALSE %in% last_window){
      last_window <- c()
    }else{
      last_window <- c(last_window,TRUE)
    }
  }

  return(df[df$ProxyBins==peakEnd,]$bins)


  # Ending_bin <- max
  # last_window <- c()
  # for(i in seq(Ending_bin,max(df$bins)-windowSize,by = windowSize)){
  #
  #   j=i
  #   k=i+(windowSize-1)
  #
  #   # while(j<=k) {
  #   #   if(j!=Ending_bin && j==i){
  #   #     if(abs(df$RateOfChangePercentage[j])<abs(df$RateOfChangePercentage[j-1])){
  #   #       peakEnd <- j-1
  #   #       last_window <- c()
  #   #     }
  #   #     j <- j+1
  #   #   }else{
  #   #
  #   #     if(abs(df$RateOfChangePercentage[j])>abs(df$RateOfChangePercentage[j+1])){
  #   #       peakEnd <- j
  #   #       last_window <- c(last_window,FALSE)
  #   #     }
  #   #
  #   #   }
  #   #   j <- j+ 1
  #   # }
  #
  #   while(j<k) {
  #     if(j!=PeakBin && j==i){
  #       if(df$RateOfChangePercentage[df$bins==j]<df$RateOfChangePercentage[df$bins==j-1]){
  #         peakEnd <- j-1
  #         last_window <- c()
  #       }
  #       j <- j+1
  #     }
  #
  #     if(!df$RateOfChangePercentage[df$bins==j]<df$RateOfChangePercentage[df$bins==j+1]){
  #
  #       peakEnd <- j
  #       last_window <- c(last_window,FALSE)
  #
  #     }
  #     j <- j+ 1
  #   }
  #
  #
  #   if(FALSE %in% last_window){
  #     last_window <- c()
  #     break
  #   }else{
  #     last_window <- c(last_window,TRUE)
  #   }
  # }
  #
  # return(peakEnd)

}


CalculatePeakRange <- function(data,control,downStream,upStream,bpSize,outdir){

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



      peakAdvanceDf <- df[1:(df[df[[sampleName]]==max(df[[sampleName]]),]$bins),]
      colnames(peakAdvanceDf) <- c("bin labels","bins",sampleName)

      peakRetreatDf <- df[(df[df[[sampleName]]==max(df[[sampleName]]),]$bins):nrow(df),]
      peakRetreatDf <- peakRetreatDf[order(-peakRetreatDf$bins),]
      colnames(peakRetreatDf) <- c("bin labels","bins",sampleName)


      browser()
      peakAdvanceDf <- CalculateRateOfChange(peakAdvanceDf,sampleName,windowSize)
      peakRetreatDf <- CalculateRateOfChange(peakRetreatDf,sampleName,windowSize)

      peakStart <- CalculatePeakStart(peakAdvanceDf,windowSize)
      peakEnd <- CalculatePeakEnd(peakRetreatDf,windowSize)


      sampleRow <- data.frame(Sample=sampleName,
                              PeakBin=df[df[,3]==max(df[,3]),]$bins,
                              PeakStartBin=peakStart,
                              PeakEndBin=peakEnd,
                              PeakWidth=(peakEnd-peakStart)*bpsPerBin,
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

    write_tsv(resultTable,file = "C:/Users/SuryadHN/Downloads/Results.tsv",col_names = TRUE, append = TRUE)

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

