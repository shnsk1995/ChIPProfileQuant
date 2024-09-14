
CalculateUpRanges <- function(data){

  binLabs  <-  data$`bin labels`
  binLabsComplete  <-  binLabs[complete.cases(binLabs)]

  upStreamRange  <-  binLabsComplete[3]
  if(any(grepl("K",upStreamRange))){
    intBpsUpStream  <-  as.numeric(abs(as.numeric(sub("\\K.*","",upStreamRange)))) * 1000
  }else{
    intBpsUpStream  <-  as.numeric(abs(as.numeric(sub("b.*","",upStreamRange))))
  }

  return(intBpsUpStream)

}


CalculateDownRanges <- function(data){

  binLabs  <-  data$`bin labels`
  binLabsComplete  <-  binLabs[complete.cases(binLabs)]

  downStreamRange  <-  binLabsComplete[1]
  if(any(grepl("K",downStreamRange))){
    intBpsDownStream  <-  as.numeric(abs(as.numeric(sub("\\K.*","",downStreamRange)))) * 1000
  }else{
    intBpsDownStream  <-  as.numeric(abs(as.numeric(sub("b.*","",downStreamRange))))
  }

  return(intBpsDownStream)

}



CalculateTotalBins <- function(data){

  binLabs  <-  data$`bin labels`
  binLabsComplete  <-  binLabs[complete.cases(binLabs)]
  return(nrow(data)/(length(binLabsComplete)/3))

}
