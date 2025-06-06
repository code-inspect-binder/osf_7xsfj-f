##############################################
## Haegeli et al. (2021) - Analysis script  ##
## FEB 25, 2021                             ##
##############################################

## Required libraries
if (!require("sjPlot")) {install.packages("sjPlot"); require("sjPlot")}
if (!require("glmmTMB")) {install.packages("glmmTMB"); require("glmmTMB")}
if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}


## Parameter setting for output
## ****************************

WriteToFile <- T                    ## Set to T is you want the plots and model output written to files
OutputFile <- "AnalysisOutput.txt"  ## Name of output file

ConfInt <- 0.95
options(contrasts = c("contr.sum", "contr.sum"))


## Data Preparation
## ****************

## Loading data
load("AvProbDF.rda")
Data <- AvProbDF

head(Data)
dim(Data)
str(Data)

summary(Data)
as.data.frame(table(Data$REGION))

## Adding MtnRange column
Data$MtnRange[Data$REGION %in% c("Northwest - BC", "Northwest Coastal", "Northwest Inland")] <- "Coast - North"
Data$MtnRange[Data$REGION %in% c("South Coast", "Sea-to-Sky", "South Coast Inland", "South Coast - Inland", "Sea-to-Sky")] <- "Coast - South"
Data$MtnRange[Data$REGION %in% c("North Columbia - Cariboos", "North Columbia - Monashees & Selkirks", "Cariboos", "North Columbia")] <- "Columbia - North"
Data$MtnRange[Data$REGION %in% c("South Columbia", "Purcells", "Kootenay Boundary")] <- "Columbia - South"
Data$MtnRange[Data$REGION %in% c("Banff, Yoho and Kootenay", "Jasper", "Kananaskis Country")] <- "Rockies - North"
Data$MtnRange[Data$REGION %in% c("Lizard Range", "South Rockies")] <- "Rockies - South"
Data$MtnRange <- factor(Data$MtnRange, levels = c("Coast - North", "Coast - South", "Columbia - North", "Columbia - South", "Rockies - North", "Rockies - South"))

table(Data$REGION, Data$MtnRange, useNA = "always")
table(Data$MtnRange, Data$Season, useNA = "always")

## Calculating averaged climate index for pacific oscillations
round(cor(Data[, c(14:17)]),2)
Data$PO <- (Data$MEI + Data$PDO + Data$PNA)/3
head(Data)
dim(Data)

## Adding correction factos for AvCan for seasons 2010-12
Data$PolCorr <- 0
Data$PolCorr[Data$Season <= 2012 & Data$MtnRange != "Rockies - North"] <- 1

table(Data$Season, Data$PolCorr)
table(Data$MtnRange, Data$PolCorr)

## Tweaking of prevalence values because beta regression doesn't like 0- and 1-proportions
## Converts zeros to very small values; trick: simple conversion formula, see e.g.,
## browseURL("https://www.jstatsoft.org/article/view/v034i02/v34i02.pdf") 
## Smithson M, Verkuilen J (2006). A Better Lemon Squeezer? Maximum-Likelihood Regression with Beta-Distributed Dependent Variables. Psychological Methods, 11(1), 54–71. 
convert01 <- function(y) {
  n <- length(y)
  y2 <- (y*(n-1) + 0.5)/n
  return(y2)
}

round(apply(Data[,c(2:11)], 2, range, na.rm = TRUE), 3)    ## minimum and maximum proportions
DataNew <- apply(Data[,c(2:11)], 2, convert01)             ## apply proportion conversion formula
DataBeta <- cbind(Data[,-c(2:11)], DataNew)                ## additional data cleaning

head(DataBeta)
round(apply(DataBeta[,c(11:20)], 2, range, na.rm = TRUE), 3)
rm(DataNew)
head(DataBeta)


## Quick descriptives
## ******************
hazard <- names(DataBeta)[11:20]      ## Extracting hazard names 
hazard
length(hazard)                        ## 9 problems + NoProb in total

op <- par(mfrow = c(5,2))
for (i in 1:10) hist(DataBeta[, hazard[i]], xlab = paste(hazard[i]), main = paste("Problem:", hazard[i]))
par(op)
rm(op)


## Fitting of mixed effects models
## *******************************

## BTL: Getting data
DataBeta_Btl <- DataBeta[DataBeta$ELEVATION == "Btl",]            ## filter Btl

## BTL: Fitting main effects models
beta_fit_Btl_main <- as.list(rep(NA, length(hazard)))       ## initialize object that stores all model fits
names(beta_fit_Btl_main) <- hazard

for (i in 1:length(hazard)) {
  if (hazard[i] == "Storm" | hazard[i] == "Wind") {
    ff <- as.formula(paste(hazard[i], "~ AO + PO + MtnRange + PolCorr + (1|Season)"))   ## model with PolCorr variable
  } else {
    ff <- as.formula(paste(hazard[i], "~ AO + PO + MtnRange + (1|Season)"))             ## model without PolCorr variable
  }
  fit <- glmmTMB(ff, family = beta_family(link = "logit"), data = DataBeta_Btl)         ## fit beta regressions
  beta_fit_Btl_main[[i]] <- fit                                                         ## store models in list
}

## BTL: Fitting interaction models
beta_fit_Btl_int <- as.list(rep(NA, length(hazard)))       ## initialize object that stores all model fits
names(beta_fit_Btl_int) <- hazard

for (i in 1:length(hazard)) {
  if (hazard[i] == "Storm" | hazard[i] == "Wind") {
    ff <- as.formula(paste(hazard[i], "~ AO*MtnRange + PO*MtnRange + PolCorr + (1|Season)"))   ## model with PolCorr variable
  } else {
    ff <- as.formula(paste(hazard[i], "~ AO*MtnRange + PO*MtnRange + (1|Season)"))             ## model without PolCorr variable
  }
  fit <- glmmTMB(ff, family = beta_family(link = "logit"), data = DataBeta_Btl)                ## fit beta regressions
  beta_fit_Btl_int[[i]] <- fit                                                                 ## store models in list
}


## ALP/TL: Getting data
DataBeta_AlpTl <- DataBeta[DataBeta$ELEVATION != "Btl",]            ## filter Alp/Tl
DataBeta_AlpTl$ELEVATION <- droplevels(DataBeta_AlpTl$ELEVATION)     

## ALP/TL: Fitting main effects models
beta_fit_AlpTl_main <- as.list(rep(NA, length(hazard)))       ## initialize object that stores all model fits
names(beta_fit_AlpTl_main) <- hazard

for (i in 1:length(hazard)) {
  if (hazard[i] == "Storm" | hazard[i] == "Wind") {
    ff <- as.formula(paste(hazard[i], "~ AO + PO + MtnRange + ELEVATION + PolCorr + (1|Season)"))   ## model with PolCorr variable
  } else {
    ff <- as.formula(paste(hazard[i], "~ AO + PO + MtnRange + ELEVATION + (1|Season)"))             ## model without PolCorr variable
  }
  fit <- glmmTMB(ff, family = beta_family(link = "logit"), data = DataBeta_AlpTl)                   ## fit beta regressions
  beta_fit_AlpTl_main[[i]] <- fit                                                                   ## store models in list
}


## ALP/TL: Fitting interaction models
beta_fit_AlpTl_int <- as.list(rep(NA, length(hazard)))       ## initialize object that stores all model fits
names(beta_fit_AlpTl_int) <- hazard

for (i in 1:length(hazard)) {
  if (hazard[i] == "Storm" | hazard[i] == "Wind") {
    ff <- as.formula(paste(hazard[i], "~ AO*MtnRange + PO*MtnRange + ELEVATION + PolCorr + (1|Season)"))   ## model with PolCorr variable
  } else {
    ff <- as.formula(paste(hazard[i], "~ AO*MtnRange + PO*MtnRange + ELEVATION + (1|Season)"))             ## model without PolCorr variable
  }
  fit <- glmmTMB(ff, family = beta_family(link = "logit"), data = DataBeta_AlpTl)                          ## fit beta regressions
  beta_fit_AlpTl_int[[i]] <- fit                                                                           ## store models in list
}

rm(ff,fit, i)

## ----------- residual checks ----------
require("DHARMa")
## doing them for the interaction models only (no need to do them for the main effects as they would look the same)
length(beta_fit_Btl_int)
length(beta_fit_AlpTl_int)

## uses simulated quantile residuals and produces a Q-Q-plot for each model
dev.new()
op <- par(mfrow = c(3,4))
set.seed(123)
for (i in 1:length(beta_fit_Btl_int)) {
  simout <- simulateResiduals(fittedModel = beta_fit_Btl_int[[i]])
  plotQQunif(simout, testUniformity = TRUE, testOutliers = FALSE, testDispersion = FALSE)  
  ## show KS-test only (could be turned off too, not that important)
}
par(op)            ## The second last model is a bit off, all the other ones look OK

dev.new()
op <- par(mfrow = c(3,4))
set.seed(123)
for (i in 1:length(beta_fit_AlpTl_int)) {
  simout <- simulateResiduals(fittedModel = beta_fit_AlpTl_int[[i]])
  plotQQunif(simout, testUniformity = TRUE, testOutliers = FALSE, testDispersion = FALSE)  
}
par(op)            ## they all look satisfactory


## Support functions for model examination
## **************************************

## Comparing of estimated marginal means
compareRangesWithPvalues <- function(model, pvalue = 0.05) {
  
  AO_quant <- c(min(model$frame$AO), max(model$frame$AO))  ## Min and max values
  PO_quant <- c(min(model$frame$PO), max(model$frame$PO))
  
  grid <- ref_grid(model, at = list(AO = AO_quant, PO = PO_quant), transform = "response")  ## some pre-processing
  
  emm2 <- emmeans(grid, specs = list("AO", "PO"), by = "MtnRange")  ## computing expected proportions
  comps <- pairs(emm2, adjust = "holm") ## doing pairwise comparisons (Holm corrected p-values)
  
  AO_comps <- summary(comps[[1]])    ## AO output
  PO_comps <- summary(comps[[2]])    ## PO output
  
  cat("\nDifferences AO with post-hoc test\n")
  for (i in 1:nrow(AO_comps)){
    cat(paste0(AO_comps$MtnRange[i], ": ", 
               format(round(-100*AO_comps$estimate[i], 1), nsmall = 1), 
               "pp (p-value = ", format(round(AO_comps$p.value[i], 3), nsmall = 3),")",
               ifelse(AO_comps$p.value[i] < 0.001, " ***\n", ifelse(AO_comps$p.value[i] < 0.01, " **\n", ifelse(AO_comps$p.value[i] < 0.05, " *\n", ifelse(AO_comps$p.value[i] < 0.1, " .\n", "\n"))))))
  }
  cat("\nDifferences PO with post-hoc test\n")
  for (i in 1:nrow(PO_comps)){
    cat(paste0(PO_comps$MtnRange[i], ": ", 
               format(round(-100*PO_comps$estimate[i], 1), nsmall = 1), 
               "pp (p-value = ", format(round(PO_comps$p.value[i], 3), nsmall = 3),")",
               ifelse(PO_comps$p.value[i] < 0.001, " ***\n", ifelse(PO_comps$p.value[i] < 0.01, " **\n", ifelse(PO_comps$p.value[i] < 0.05, " *\n", ifelse(PO_comps$p.value[i] < 0.1, " .\n", "\n"))))))
  }
  
}



## Formatting of output
compareRanges <- function(predDF, MtnRanges) {
  
  for (i in 1:length(MtnRanges)) {
    
    MtnRng <- MtnRanges[i]
    MinEst <- predDF$pred[predDF$x == min(predDF$x) & predDF$grp == MtnRng]
    MinHigh <- predDF$high[predDF$x == min(predDF$x) & predDF$grp == MtnRng]
    MinLow <- predDF$low[predDF$x == min(predDF$x) & predDF$grp == MtnRng]
    MaxEst <- predDF$pred[predDF$x == max(predDF$x) & predDF$grp == MtnRng]
    MaxHigh <- predDF$high[predDF$x == max(predDF$x) & predDF$grp == MtnRng]
    MaxLow <- predDF$low[predDF$x == max(predDF$x) & predDF$grp == MtnRng]
    
    string <- paste0(MtnRng, 
                     ": ", 
                     format(round(MinEst,2), nsmall = 2), 
                     "[", format(round(MinLow,2), nsmall = 2), 
                     "-" , format(round(MinHigh,2), nsmall = 2), 
                     "] vs. ",
                     format(round(MaxEst,2), nsmall = 2), 
                     "[", format(round(MaxLow,2), nsmall = 2), 
                     "-" , format(round(MaxHigh,2), nsmall = 2), 
                     "] --> ",
                     format(round(MaxEst - MinEst,2), nsmall = 2), 
                     ifelse(((MinEst < MaxLow) & (MaxEst > MinHigh)), " ***", ""),
                     ifelse(((MinEst > MaxHigh) & (MaxEst < MinLow)), " ***", ""))
    
    cat(paste0((string), "\n"))
  }
  
}


## Loop for model examination
## **************************

model_vec <- hazard 
names(model_vec) <- 1:length(model_vec)
model_vec

MtnRanges <- c("Coast - North", "Coast - South", "Columbia - North", "Columbia - South", "Rockies - North", "Rockies - South")

Main <- c("Storm slab", "Wind slab", "Persistent slab", "Deep pers. slab", "All pers. slab", "Wet slab", 
          "Wet loose avalanche", "Dry loose avalanche", "Cornice avalanche", "No avalanche problem")

if (WriteToFile) sink(OutputFile, split = T)

for (model_ind in 1:length(model_vec)) {

  cat(paste0("\n=================================", 
             "\n=== ", model_ind, ") MODELS FOR ", toupper(model_vec[model_ind]), " ===",
             "\n=================================\n\n"))

  ## Get models
  fit_main_Alp <- beta_fit_AlpTl_main[[model_ind]]
  fit_int_Alp <- beta_fit_AlpTl_int[[model_ind]]
  fit_main_Btl <- beta_fit_Btl_main[[model_ind]]
  fit_int_Btl <- beta_fit_Btl_int[[model_ind]]
  
  ## ALP/TL: Choose models
  cat(paste0(toupper(model_vec[model_ind]), ": ALPINE/TREELINE LIKELIHOOD TEST", "\n*****************************************\n\n"))
  print(anova(fit_main_Alp, fit_int_Alp))
  
  if (anova(fit_main_Alp, fit_int_Alp)$`Pr(>Chisq)`[2] < 0.05) {
    fit_Alp <- fit_int_Alp
  } else {
    fit_Alp <- fit_main_Alp
  }
  
  ## ALP/TL: Print summaries and differences at the end of the ranges
  cat(paste0("\n\n", toupper(model_vec[model_ind]), ": ALPINE/TREELINE FINAL MODEL", "\n*************************************\n\n"))
  print(summary(fit_Alp))
  compareRangesWithPvalues(fit_Alp)
  
  
  ## BTL: Choose models
    cat(paste0("\n\n", toupper(model_vec[model_ind]), ": BELOW TREELINE LIKELIHOOD TEST", "\n***************************************\n\n"))
  print(anova(fit_main_Btl, fit_int_Btl))
  
  if (anova(fit_main_Btl, fit_int_Btl)$`Pr(>Chisq)`[2] < 0.05) {
    model_Btl <- "int"
    fit_Btl <- fit_int_Btl
  } else {
    model_Btl <- "main"
    fit_Btl <- fit_main_Btl
  }
  
  ## BTL: Print summaries and differences at the end of the ranges
  cat(paste0("\n\n", toupper(model_vec[model_ind]), ": BELOW TREELINE FINAL MODEL", "\n***********************************\n\n"))
  print(summary(fit_Btl))
  compareRangesWithPvalues(fit_Btl)
  
  ## Cleanup
  rm(fit_main_Alp, fit_int_Alp, fit_Alp)
  rm(fit_main_Btl, fit_int_Btl, fit_Btl)

} 

## Close sink
if(WriteToFile) sink()

## Cleanup
rm(model_ind, model_vec)
rm(MtnRanges, hazard, Main)

## Save models
save.image("AnalysisWorkspace.RData")



