##############################################
## Haegeli et al. (2020) - Table 1          ##
## May 28, 2020                             ##
##############################################

## Getting data
load("AvProbDF.rda")

## Summaries
summary(AvProbDF[AvProbDF$ELEVATION == "Alp",])
summary(AvProbDF[AvProbDF$ELEVATION == "Tl",])
summary(AvProbDF[AvProbDF$ELEVATION == "Btl",])