##############################################
## Haegeli et al. (2020) - Table 3          ##
## May 28, 2020                             ##
##############################################

## Getting data
load("AvProbDF.rda")

## Avalanche problem array
AvProbs <- colnames(AvProbDF)[2:11]

## Output DF
CorSummary <- data.frame(AvProb = AvProbs,
                         AlpTl = NA,
                         AlpBtl = NA,
                         TlBtl = NA)

## Calculation of correlations
for (i in 1:length(AvProbs)) {

  AvProb <- AvProbs[i]
  
  TempAlp <- AvProbDF[AvProbDF$ELEVATION == "Alp", c("Season", "REGION", AvProb)]
  names(TempAlp)[3] <- "Alp"
  TempTl <- AvProbDF[AvProbDF$ELEVATION == "Tl", c("Season", "REGION", AvProb)]
  names(TempTl)[3] <- "Tl"
  TempBtl <- AvProbDF[AvProbDF$ELEVATION == "Btl", c("Season", "REGION", AvProb)]
  names(TempBtl)[3] <- "Btl"
  
  Temp <- merge(TempAlp, TempTl, by = c("Season", "REGION"))
  Temp <- merge(Temp, TempBtl, by = c("Season", "REGION"))
  rm(TempAlp, TempTl, TempBtl)
  
  CorTbl <- round(cor(Temp[,c(3:5)]), 2)

  CorSummary$AlpTl[i] <- CorTbl[1,2]
  CorSummary$AlpBtl[i] <- CorTbl[1,3]
  CorSummary$TlBtl[i] <- CorTbl[2,3]
  
  rm(Temp, CorTbl)
  
}

## Cleanup
rm(i, AvProb)

## Correlations values
CorSummary
